# matching FIRE institutions to NSSE institutions
library(dplyr)
library(readxl)
library(odbc)
library(DBI)
library(stringr)
library(purrr)
library(tidyr)
library(haven)

# retrievable from https://nces.ed.gov/ipeds/use-the-data/download-access-database; converted to SQLite for cross-platform compatibility
ipeds <- dbConnect(RSQLite::SQLite(),
                   dbname = "C:/IPEDS/sqlite/IPEDS_202324.db")

# 2020 is first year of FIRE survey, but not all scales go back that far or were necessarily constructed the same way.
# The "kiosk" table simply lists NSSE participation by year, like this: https://nsse.indiana.edu/support-resources/institution-search/index.html
kiosk <- read_xlsx("C:/nsse/2025/nsse kiosk (2013-25).xlsx") |>
  filter(admin_year >= 2020)

fire_insts <- readRDS("data/fire_data.rds") |>
  pluck("fire_survey") |>
  distinct(school_name, survey_year, school_state)

# for str_replace_all(); try to keep everything standard: Indiana University Bloomington, University of Ohio Main Campus, etc.
replacements <- c(
  # St. or St, not necessarily at the beginning e.g U. of St John
  "St\\.*\\s{1}" = "Saint ",
  "&" = "and",
  "^The " = "",
  # possessives out
  "'s" = "s",
  # spaced dash to dash
  " - " = "-",
  # em dash to dash
  "—" = "-",
  # dash to space
  "-" = " ",
  # in or at etc. to blank
  " at " = " ",
  " in " = " ",
  # commas too
  ", " = " ",
  # abbreviation
  "\\." = "")

inst_matches <- list()
# unaltered matches ####
inst_matches$raw <- inner_join(fire_insts, kiosk,
                               by = c("school_name" = "name_report",
                                      "survey_year" = "admin_year",
                                      "school_state" = "state_name"))

# standardized NSSE matches ####
inst_matches$nsse <- list(fire_insts, kiosk) |>
  # coerce names for simplicity
  map_at(2, rename, school_name = name_report, survey_year = admin_year,
         school_state = state_name) |>
  map(mutate, school_name = str_squish(str_replace_all(school_name, replacements))) |>
  reduce(inner_join, by = c("school_name", "survey_year", "school_state"))

# standardized IPEDS matches ####
inst_matches$ipeds <- list(fire_insts,
                           tbl(ipeds, "HD2023") |>
                             select(school_name = INSTNM, STABBR, unitid = UNITID) |>
                             collect() |>
                             # HD table includes only state abb; supplement with built-in R 50 states
                             # this will miss any institution in DC, or outlying areas.
                             # manual inspection shows only school_state not in state.name is DC.
                             left_join(
                               data.frame(STABBR = state.abb, school_state = state.name) |>
                                 bind_rows(tibble(STABBR = c("DC"),
                                                  school_state = c("District of Columbia"))
                                 ),
                               by = "STABBR") |>
                             select(school_name, school_state, unitid)
) |>
  map(mutate, school_name = str_squish(str_replace_all(school_name, replacements))) |>
  reduce(inner_join, by = c("school_name", "school_state"))

# who's not matched from IPEDS? Those w/ or w/o cities in names
anti_join(
  mutate(fire_insts,
         school_name = str_squish(str_replace_all(school_name, replacements))),
  inst_matches$ipeds,
  by = c("school_name", "survey_year", "school_state")) |>
  distinct(school_name)

# how many good matches? for IPEDS UNITIDs? 45-80%, with most (predictably) being in IPEDS.
map(inst_matches, distinct, unitid) |>
  map(\(x) nrow(x) / 257 * 100)

# how many actually have done NSSE since 2023? 125, or 162 total NSSE administrations
map(inst_matches, distinct, school_name, unitid) |>
  reduce(full_join, by = c("school_name", "unitid")) |>
  distinct(school_name, unitid) |>
  nest_join(
    select(kiosk, unitid, admin_year) |>
      filter(admin_year >= 2023),
    by = "unitid", name = "nsse") |>
  unnest(nsse) |>
  distinct(unitid, .keep_all = TRUE)

# How many did FIRE and NSSE in the last 3 years (NSSE23 - NSSE25)
# ~half did not (but may have done NSSE in the past), and about even split between same last joint admin, within 1 year, and within 2 years. No case where inst did NSSE more recently than FIRE.
x <- map(inst_matches, distinct, school_name, unitid) |>
  reduce(full_join, by = c("school_name", "unitid")) |>
  distinct(school_name, unitid) |>
  left_join(
    filter(kiosk, admin_year >= 2023) |>
      summarize(last_nsse = max(admin_year), .by = unitid),
    by = "unitid") |>
  left_join(
    summarize(fire_insts, last_fire = max(survey_year), .by = school_name),
    by = "school_name"
  ) |>
  mutate(admin_year_diff = last_nsse - last_fire)

count(x, admin_year_diff) |>
  mutate(p = n / sum(n)  * 100)

# if we dont' care about duplicate schools but same-year administrations...
x <- map(inst_matches, distinct, school_name, unitid) |>
  reduce(full_join, by = c("school_name", "unitid")) |>
  distinct(school_name, unitid) |>
  nest_join(
    filter(kiosk, admin_year >= 2023) |>
      select(unitid, admin_year),
    by = "unitid", name = "nsse") |>
  nest_join(
    filter(fire_insts, survey_year >= 2023) |>
      select(school_name, survey_year),
    by = "school_name", name =  "fire"
  ) |>
  mutate(fire_nsse = map2(nsse, fire, left_join,
                          by = c("admin_year" = "survey_year")),
         n_fire_nsse = map_int(fire_nsse, nrow))

# ...0 had no same-year overlap, 118 had one, 12 had two, 3 had 7 - so not much is gained by including the 19 administrations
count(x, n_fire_nsse)

# so either 162 same-year administrations or 125 total unique same-year admins.
unnest(x, fire_nsse) |>
  distinct(unitid)

# manual imputation ####
# small enough, most of these probably refer to the flagships
# FIRE's ranking page has campuses listed as rankings.thefire.org/campus/UNITID-name), which can be useful for manual inspection

unitid_manual <- left_join(
  mutate(fire_insts,
         school_name_clean = str_squish(str_replace_all(school_name, replacements))),
  map(inst_matches, distinct, school_name, unitid) |>
    reduce(full_join, by = c("school_name", "unitid")) |>
    distinct(school_name, unitid),
  by = c("school_name_clean" = "school_name")) |>
  left_join(
    mutate(kiosk, unitid = unitid, admin_year = admin_year, NSSE = 1,
           .keep = "none"),
    by = c("unitid", "survey_year" = "admin_year")) |>
  filter(is.na(unitid)) |>
  distinct(school_name, school_state) |>
  mutate(unitid = case_when(
    school_name == "The University of Alabama Tuscaloosa" ~ 100751,
    school_name == "University of Alaska" ~ 102553, # Anchorage, guess.
    school_name == "Arizona State University" ~ 104151, # Campus immersion
    school_name == "California Polytechnic State University" ~ 110422, # San Luis Obispo
    school_name == "University of Hawaii" ~ 141574, # U HI at Manoa, in Honolulu
    school_name == "Indiana University" ~ 151351, # IUB
    school_name == "Louisiana State University" ~ 159391, # Baton Rouge, tech. A&M
    school_name == "University of Maryland" ~ 163286, # Baltimore
    school_name == "University of Massachusetts" ~ 166629, # Amherst
    school_name == "University of Michigan" ~ 170976, # Ann Arbor
    school_name == "University of Minnesota" ~ 174066, # Twin Cities/Minneapolis
    school_name == "University of New Hampshire" ~ 183044, #Durham/main campus
    school_name == "Rutgers University" ~ 186380, # new brunswick
    school_name == "The University of New Mexico" ~ 187985, # albuquerque/main campus
    school_name == "Columbia University" ~ 190150, # in CUNY
    school_name == "University at Albany - State University of New York" ~ 196060, #w/o SUNY
    school_name == "University at Buffalo - State University Of New York" ~ 196088,
    school_name == "North Carolina A&T State University" ~ 199102, # "A & T", Greensboro, shouldve matched?
    school_name == "North Carolina State University" ~ 199193, # raleigh
    school_name == "Kent State University" ~ 203517, # at Kent, OH
    school_name == "Miami University" ~ 204024, # Oxford OH
    school_name == "Oklahoma State University" ~ 207388, # main
    school_name == "The University of Oklahoma" ~ 207500, # norman
    school_name == "Pennsylvania State University" ~ 214777, # main cmpuas
    school_name == "University of Pittsburgh" ~ 215293, # pittsburgh campus
    school_name == "University of South Carolina" ~ 218663, # Columbia
    school_name == "Texas A&M University" ~ 228723, # given as college station
    school_name == "College of William & Mary" ~ 231624, # just William and Mary
    school_name == "Virginia Tech University" ~ 233921, # blacksbugr
    school_name == "The University of Virginia" ~ 234076, # main campus
    school_name == "University of Washington" ~ 236948, # seattle
    school_name == "Purdue University" ~ 243780, # main campus
  ))

# tidy up matching ####
# add manual lookup unitid's, those found from raw/NSSE/IPEDS, then connect to NSSE admin's.
# Now these can connect to student data via school_name and to IPEDS via unitid.
fire_insts <- left_join(
  mutate(fire_insts,
         school_name_clean = str_squish(str_replace_all(school_name, replacements))),
  unitid_manual,
  by = c("school_name", "school_state")) |>
  left_join(
    map(inst_matches, distinct, school_name, unitid) |>
      reduce(full_join, by = c("school_name", "unitid")) |>
      distinct(school_name, unitid),
    by = c("school_name_clean" = "school_name")) |>
  mutate(unitid = if_else(!is.na(unitid.x), unitid.x, unitid.y),
         unitid.x = NULL,
         unitid.y = NULL) |>
  left_join(
    mutate(kiosk, unitid = unitid, admin_year = admin_year, NSSE = 1,
           .keep = "none"),
    by = c("unitid", "survey_year" = "admin_year")) |>
  select(school_name, survey_year, school_state, unitid, NSSE)

# quick facts ####
# about 20% of all FIRE admins had concurrent year NSSE admin (229)
count(fire_insts, NSSE) |>
  mutate(p = n / sum(n) * 100)

# 149 unique institutions of 257 (58%)
filter(fire_insts, NSSE == 1) |>
  distinct(unitid, .keep_all = TRUE) |>
  count(NSSE)

# of recent cases (with ~same FIRE KPIs): 160 total administrations
filter(fire_insts, survey_year >= 2023) |>
  count(survey_year, NSSE)

# and total recent unique institutions: 135, decent spread over years.
filter(fire_insts, survey_year >= 2023 & NSSE == 1) |>
  distinct(unitid, .keep_all = TRUE) |>
  count(survey_year, NSSE)

# topical module coverage: primarily Academic Advising, Career & Workforce...
left_join(fire_insts,
          select(kiosk, unitid, admin_year, module1abb, module2abb),
          by = c("unitid", "survey_year" = "admin_year")
) |>
  filter(survey_year >= 2023 & NSSE == 1) |>
  pivot_longer(contains("module")) |>
  count(value, sort = TRUE)

# unique institutions...decent amount still for AAD, CWP, ICD, MHW.
left_join(fire_insts,
          select(kiosk, unitid, admin_year, module1abb, module2abb),
          by = c("unitid", "survey_year" = "admin_year")
) |>
  filter(survey_year >= 2023 & NSSE == 1) |>
  distinct(unitid, .keep_all = TRUE) |>
  pivot_longer(contains("module")) |>
  count(value, sort = TRUE)

fire_insts <- left_join(
  fire_insts,
  select(kiosk, unitid, admin_year),
  by = c("unitid", "survey_year" = "admin_year")
)


# export ####
data <- readRDS("data/fire_data.rds")

cat(file.size("data/fire_data.rds") / 1e6, "Mb")

# add fire_insts table with unitid and NSSE flag to FIRE data
list("fire_codebook" = data$codebook,
     "fire_survey" = data$fire_survey,
     "fire_scales" = data$fire_scales,
     "fire_insts" = fire_insts
) |>
  saveRDS("data/fire_data.rds")

# after, still small, 1/10th size of than giant FIRE JSON
cat(file.size("data/fire_data.rds") / 1e6, "Mb")
cat(file.size("data/fire_survey_data_all.json") / 1e6, "Mb")


dbDisconnect(ipeds)
