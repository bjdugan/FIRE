# import and clean FIRE data
library(dplyr)
library(readr)
library(readxl)
library(jsonlite)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)

## codebooks ####
# retrievable by downloading a single institution's data, e.g. https://rankings.fire.org/methodology#data-download
codebook <- dir("data", pattern = "codebook", full.names = TRUE) |>
  read_csv(show_col_types = FALSE) |>
  rename(item = Variable, label = VariableLabel, value = Value,
         response_option = ValueLabel) |>
  filter(!is.na(item) | !is.na(value)) |>
  # these are left blank in codebook
  fill(item, label) |>
  nest("response_options" = c(value, response_option), .by = c(item, label))

# add some organization for variables
codebook <- mutate(codebook,
                   section = case_when(
                     row_number() < grep("regvoter", codebook$item) ~ "metadata",
                     row_number() >= grep("regvoter", codebook$item) &
                       row_number() <= grep("major_group", codebook$item)
                     ~ "demographics",
                     row_number() >= grep("hideopforgrade", codebook$item)
                     ~ "survey"))

# add topics FIRE details in methodology for grouping student perception items, six of the twelve components used in their College Free Speech Rankings (CFSR).
# The items do not appear to be explicitly mapped anywhere, including in the codebook, so items were grouped and matched based on their 2026 CFSR report (https://rankings.thefire.org/explore?demo=all&year=2025).
# https://osf.io/hyd6v/overview pre-registration
# https://rankings.thefire.org/methodology summary methodology

topics <- tibble(
  # six student perception components of the CFSR
  topic = c(rep("Administrative Support", 2),
            rep("Comfort Expressing Ideas", 5),
            rep("Self-Censorship", 3),
            rep("Disruptive Conduct", 3),
            rep("Political Tolerance", 9), # 6, but 3 changed in 2024
            rep("Openness", 21),
            rep("Additional Questions", 12)
  ),
  # labels
  label = c("How clear is it to you that your college administration protects free speech on campus?",
            "If a controversy over offensive speech were to occur on your campus, how likely is it that the administration would defend the speaker's right to express their views?",
            # comfort expressing ideas
            "How comfortable would you feel doing the following on your campus?: Publicly disagreeing with a professor about a controversial political topic.",
            "How comfortable would you feel doing the following on your campus? Expressing disagreement with one of your professors about a controversial political topic in a written assignment.",
            "How comfortable would you feel doing the following on your campus? Expressing your views on a controversial political topic during an in-class discussion.",
            "How comfortable would you feel doing the following on your campus? Expressing your views on a controversial political topic to other students during a discussion in a common campus space such as a quad, dining hall, or lounge.",
            "How comfortable would you feel doing the following on your campus? Expressing an unpopular political opinion to your fellow students on a social media account tied to your name.",
            # self censorship
            "How often do you self-censor during conversations with other students on campus?",
            "How often do you self-censor during conversations with your professors?",
            "How often do you self-censor during classroom discussions?",
            # disruptive conduct
            "How acceptable would you say it is for students to engage in the following actions to protest a campus speaker? Shouting down a speaker to prevent them from speaking on campus.",
            "How acceptable would you say it is for students to engage in the following actions to protest a campus speaker? Blocking other students from attending a campus speech.",
            "How acceptable would you say it is for students to engage in the following actions to protest a campus speaker? Using violence to stop a campus speech.",
            # poltical tolerance: conservative speakers (all years)
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? Transgender people have a mental disorder.",
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? Abortion should be completely illegal.",
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? Black Lives Matter is a hate group.",
            # political tolerance: liberal speakers (>= 2024)
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? The Catholic church is a pedophilic institution.",
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? The police are just as racist as the Ku Klux Klan.",
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? Children should be able to transition without parental consent.",
            # political tolerance: liberal speakers (< 2024)
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? The Second Amendment should be repealed so that guns can be confiscated.",
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? Religious liberty is used as an excuse to discriminate against gays and lesbians.",
            "Student groups often invite speakers to campus to express their views on a range of topics. Regardless of your own views on the topic, should your school ALLOW or NOT ALLOW a speaker on campus who has previously expressed the following idea? Structural racism maintains inequality by protecting White privilege.",
            # openness
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Abortion",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Affirmative action",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): China",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Climate change",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Crime",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Economic inequality",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Freedom of speech",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Gay rights",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Gender inequality",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Gun control",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Hate speech",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Immigration",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): The Israeli/Palestinian conflict",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Police misconduct",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): The 2024 presidential election",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Racial inequality",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Religion",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): The Supreme Court",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Sexual assault",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): Transgender rights",
            "Some students say it can be difficult to have conversations about certain issues on campus. Which of the following issues, if any, would you say are difficult to have an open and honest conversation about on your campus? (select all that apply): None of the above",
            # addt'l questions
            "How often, if at all, do you hide your political beliefs from your professors in an attempt to get a better grade?",
            "How likely or unlikely is it that a student on campus would be reported to the administration by another student for saying something controversial?",
            "How likely or unlikely is it that a professor on campus would be reported to the administration by a student for saying something controversial?",
            "Have you ever been disciplined by your college’s administration for expression on campus?",
            "Have you or anyone you know filed a Title IX complaint?",
            "Has a Title IX complaint ever been filed against you or someone you know?",
            "How often would you say that you feel anxious?",
            "How often would you say that you feel lonely or isolated?",
            "How often would you say that you feel like you have no time for yourself?",
            "How often would you say that you feel depressed?",
            "How often would you say that you feel stressed, frustrated, or overwhelmed?",
            "On your campus, how often have you felt that you could not express your opinion on a subject because of how students, a professor, or the administration would respond?"
  )
)

# ensure alignment with codebook: labels ought to match for each item in the topics table (the codebooks include a number of metadata variables or other items from past-year or other survey blocks).

# should return an empty table
anti_join(topics, codebook, by = "label")
# should return a single match for each label in topics
inner_join(topics, codebook, by = "label")

# add topics to codebook, and identify scales for each topic; similar to NSSE, most variables share prefix indicating the scale and topic, e.g. "sc_*" for items in Self-Censorship.
codebook <- left_join(codebook, topics, by = "label") |>
  # ignore addt'l q etc.
  mutate(scale = case_when(
    (topic == "Additional Questions" | is.na(topic)) ~ NA_character_,
    grepl("admin", item) ~ "admin", # not [scale]_[item]
    TRUE ~ str_split_i(item, "_", 1)),
    # sc exists in data but is not considered part of the Self-Censorship scale; this avoids confusion
    scale = if_else(!is.na(scale), paste0(scale, "_scale"), NA_character_)
  )

# scale parameters as described in "Calculating the College Free Speech Rankings" section of pre-registration and the same section on their website https://rankings.thefire.org/methodology. Table 2 of the pre-registration details the range for each scale, which generally corresponds to the number of associated items.
# spk (political tolerance scale) is calculated separated for liberal and conservatives, then averaged. The underlying items for spk_lib_scale changed in 2024.
scale_weights <- filter(codebook, item != "tk_none") |>
  count(scale, name = "ymin") |>
  filter(!is.na(scale)) |>
  mutate(
    ymax = case_when(
      scale == "cf_scale" ~ 15, # Comfort Expressing Ideas
      scale == "sc_scale" ~ 20, # Self-Censorship
      scale == "act_scale" ~ 20, # Disruptive conduct
      scale == "admin_scale" ~ 20, # Administrative Support
      scale == "tk_scale" ~ 10, # Openness
      scale == "spk_scale" ~ 15, # Political tolerance
    ),
    # in some cases the minimum score is not the number of items
    ymin = case_when(
      scale == "tk_scale" ~ 0,
      scale == "spk_scale" ~ 3,
      TRUE ~ ymin),
  ) |>
  left_join(
    distinct(codebook, scale, .keep_all = TRUE) |>
      unnest(response_options) |>
      # not missing, not "Missing" or "Not administered"
      filter(!is.na(scale) & !value %in% c(98, 99)) |>
      summarize(xmin = min(value), xmax = max(value), n_ro = length(value), .by = scale),
    by = "scale") |>
  left_join(
    filter(codebook, !is.na(scale) & item != "tk_none") |>
      count(scale),
    by = "scale"
  )

# tolerance scale spk_* is handled a bit differently
scale_weights <- bind_rows(scale_weights,
                           tibble(scale = c("spk_con_scale", "spk_lib_scale"),
                                  ymin = 3, ymax = 15, xmin = 1, xmax = 4, n_ro = 4, n = 3)
)


# align factor, needed?
# mutate(scale_weights,
#        wt = list(seq(ymin, ymax, length.out = n_ro) / seq(xmin, xmax, length.out = n_ro)),
#        .by = scale
# ) |>
#   unnest(wt)


# now codebook is complete collection of items (variable name), label (question), response options, section, topic, and scale if applicable. E.g.
filter(codebook, scale == "sc_scale") |>
  unnest(response_options) |>
  select(item, label, value, response_option)

## data ####
# retrieved as JSON from https://rankings.thefire.org/explore?demo=all&year=2025
# specific campus, state, or year data are also available
# this contains some school-level information like name, type, weights, administration year, and all student responses.


# be aware this is a large file and may take some time to load
cat(file.size("data/fire_survey_data_all.json") / 1e6, "Mb")
t1 <- Sys.time()
fire_survey <- read_json("data/fire_survey_data_all.json", simplifyVector = TRUE) |>
  pluck("data") |>
  as_tibble()
t2 <- Sys.time()
t2 - t1
gc()
# much smaller in memory than on disk
cat(object.size(fire_survey) / 1e6, "Mb")

# Unfortunately, despite `unitid` being in the codebook it is not present in the all-data file (it is as metadata in individual school json files) Adding any IPEDS data or linking to other sources like NSSE requires matching on name and other features.
ncol(select(fire_survey, contains("unitid")))

# weights are oddly recorded as character fields
select(fire_survey, contains("weight")) |>
  map(typeof)

fire_survey <- mutate(fire_survey, across(contains("weight"), as.numeric))

# quick look at relevant survey items. All missing values are coded as 99 per codebook (no NA's, max 99)
select(fire_survey,
       all_of(filter(codebook, !is.na(scale)) |>
                pull(item))) |>
  summary()

# how many valid cases each year?
select(fire_survey, survey_year,
       all_of(filter(codebook, !is.na(scale)) |>
                pull(item))) |>
  pivot_longer(-survey_year) |>
  mutate(value = case_when(value == 99 ~ "Missing",
                           value == 98 ~ "Not in survey",
                           TRUE ~ "Valid")) |>
  count(survey_year, name, value) |>
  mutate(n = n / sum(n) * 100, .by= c(name, survey_year)) |>
  pivot_wider(names_from = survey_year, values_from = n) |>
  arrange(name)

# calculate scales ####
# as described in methodology:
# "We code responses so that higher scores indicate a better free speech environment. We sum each student’s answers to the questions in each component. We then calculate the school weighted mean of each component and then scale the scores (e.g. Y = ((X - Xmin) / Xrange) * (Ymax - Ymin) + Ymin) so that the highest hypothetical score from the student perception components is 100 points."
# Note that tk_none is not included, and political tolerance items (spk_) are calculated for liberal and conservative speakers separately then averaged. Different items were used for the tolerance of liberal speakers prior to 2024: spk_guns, spk_religlib, and spk_whites, where from 2024 onward spk_cathped, spk_policekkk, and spk_kidtrans were used. See the 2024 rankings (https://www.fire.org/sites/default/files/2023/09/CFSR%202024_final_updated.pdf).

fire_scales <- select(fire_survey, response_id, survey_year,
            all_of(pull(filter(codebook, !is.na(scale)), "item"))) |>
  mutate(
    # reverse code self-censorship so that higher values = less censorship; items not asked prior to 2023.
    # e.g. 1=Never (self-censor), 1 + ((1 - 3) * -2) -> 1 + 4 = 5
    # 3=Occasionaly (mid-point), 3 + ((3 - 3) * -2) -> 3 + 0 = 3
    across(c(sc_stdnt, sc_profs, sc_class),
           \(x) if_else(!x %in% c(98, 99), x + ((x - 3) * -2), x)),
    # "Responses are coded 0 if they are difficult and 1 if not selected so that higher scores indicate fewer issues being selected" yet in codebooks, 1=Yes (difficult), so reverse-code.
    across(
       all_of(pull(filter(codebook, scale == "tk_scale" & item != "tk_none"), "item")),
       \(x) if_else(!x %in% c(98, 99), abs(x - 1), x))
  ) |>
  pivot_longer(-c(response_id, survey_year)) |>
  left_join( select(codebook, item, scale), by = c("name" = "item")) |>
  # drop missing values and not-administered values
  filter(!value %in% c(98, 99)) |>
  # political tolerance items: summed for lib/con then averaged; flag them as lib/ccon.
  mutate(scale = case_when(
    scale == "spk_scale"
    & name %in% c("spk_cathped", "spk_policekkk", "spk_kidtrans", # >=2024
                  "spk_guns", "spk_religlib", "spk_whites" # < 2024
                  )
    ~ "spk_lib_scale",
    scale == "spk_scale"
    & name %in% c("spk_trans", "spk_abortion", "spk_blm")
    ~ "spk_con_scale",
    TRUE ~ scale)) |>
  # sum student responses
  summarize(value = sum(value), .by = c(response_id, survey_year, scale)) |>
  left_join(select(fire_survey, response_id, survey_year, school_weight, school_name),
            by = c("response_id", "survey_year")) |>
  # school weighted mean and scaling
  summarize(
    value_wtd_avg = weighted.mean(value, school_weight),
    .by = c(school_name, survey_year, scale)
  ) |>
  # scaling
  left_join(scale_weights, by = "scale") |>
  # Y = ((X - Xmin) / Xrange) * (Ymax - Ymin) + Ymin)
  mutate(value_wtd_scaled =
           ((value_wtd_avg - min(value_wtd_avg)) / (max(value_wtd_avg) - min(value_wtd_avg))) *
           (ymax - ymin) + ymin,
         .by = c(survey_year, scale))

# create overall Political tolerance measure (summed and scaled separately, then averaged, less abs value of diff)
fire_scales <- bind_rows(fire_scales,
          filter(fire_scales, scale %in% c("spk_con_scale", "spk_lib_scale")) |>
            select(school_name, survey_year, scale, value_wtd_scaled) |>
            pivot_wider(names_from = scale, values_from = value_wtd_scaled) |>
            mutate(spk_scale = mean(c(spk_con_scale, spk_lib_scale)) - abs(spk_con_scale - spk_lib_scale),
                   spk_con_scale = NULL, spk_lib_scale = NULL,
                   .by = c(school_name, survey_year)) |>
            pivot_longer(spk_scale) |>
            rename(scale = name, value_wtd_scaled = value)) |>
  arrange(school_name, desc(survey_year), scale) |>
  # add scale parameters where missing
  mutate(ymin = if_else(scale == "spk_scale", 3, ymin),
         ymax = if_else(scale == "spk_scale", 15, ymax),
         xmin = if_else(scale == "spk_scale", 1, xmin),
         xmax = if_else(scale == "spk_scale", 4, xmax),
         n_ro = if_else(scale == "spk_scale", 4, n_ro),
         n = if_else(scale == "spk_scale", 6, n))

# check: no scaled, weighted school avg should be outside of the parameters given.
# All OK except for spk_scale, which doesn't seem to follow the bounds (ymin, ymax) for cases of extreme differences (122 of 1166 total). Checked by hand, probably OK...
filter(fire_scales, value_wtd_scaled < ymin | value_wtd_scaled > ymax)

# check: the highest possible score per school should not exceed 100 (and presumably no negative values).
# OK
filter(fire_scales, !scale %in% c("spk_lib_scale", "spk_con_scale")) |>
  summarize(value_wtd_scaled = sum(value_wtd_scaled), .by = c(school_name, survey_year)) |>
  filter(value_wtd_scaled >= 100 & value_wtd_scaled <= 0)

# check: compare to select scores for an institution: Indiana University Bloomington (2025 data/'26 rankings).
# https://rankings.thefire.org/assets/docs/campus/2026-151351.pdf
# should be in bottom quartile (or perhaps bottom 25 schools? unclear) for openness, self-censorship, and admin support
# OK
filter(fire_scales, survey_year == 2025 &
         scale %in% c("tk_scale", "sc_scale", "admin_scale")) |>
  mutate(pct_rank = rank(value_wtd_scaled) / n() * 100, .by = scale) |>
  arrange(desc(value_wtd_scaled)) |>
  filter(grepl("Indiana", school_name))

# add overall student perception ranking.
fire_scales <- select(fire_scales, school_name, survey_year, scale,
                             value_wtd_scaled) |>
  pivot_wider(names_from = scale, values_from = value_wtd_scaled) |>
  left_join(
    filter(fire_scales, !scale %in% c("spk_lib_scale", "spk_con_scale")) |>
      summarize(student_perception = sum(value_wtd_scaled, na.rm = TRUE),
              .by = c(school_name, survey_year)),
    by = c("school_name", "survey_year")
  )

## export ####
# save as R-native file.
list("codebook" = codebook,
     "fire_survey" = fire_survey,
     "fire_scales" = fire_scales
) |>
  saveRDS("data/fire_data.rds")

# much smaller even with codebook and summary table
cat(file.size("data/fire_data.rds") / 1e6, "Mb")
cat(file.size("data/fire_survey_data_all.json") / 1e6, "Mb")
