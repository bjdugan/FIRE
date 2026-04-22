# FIRE
## Purpose
This project presents data collected for the Foundation in Individual Rights and Expression (FIRE) 2026 College Free Speech Rankings [(CFSR)](https://www.fire.org/research-learn/2026-college-free-speech-rankings) in native R format (.rds file), complete with a data codebook, calculated measures used in the *Student Perceptions* portion of the ranking, and IPEDS `unitid` field for linking to other higher education data. 

Those interested in finding an individual school's or state's rankings should use the rankings [search](https://rankings.fire.org/) from FIRE, which requires providing an email for access. Those downloads include all of the above without the calculated scores. While it is possible to download all available data (2020-2025) for over 250 institutions of higher education in the U.S., that download does not appear include a `unitid` field making it difficult to integrate with other data (e.g. IPEDS, Carnegie Classifications).

Learn more about the FIRE CFSR [pre-registered methodology](https://osf.io/hyd6v/overview), College Pulse's (who conduct the survey) [methodology](https://collegepulse.com/methodology/) or the rankings.

## Contents
 - **01_clean fire.r** augments the included codebook with the six *Student Perceptions* topics and associated scales variables, and calculates the weighted scales for each school.  
 - **02_match fire nsse.r** matches provided identifiers (`school_name`, `school_state`) with other sources like IPEDS and National Survey of Student Engagement (NSSE) data while applying some standardization of institution names, and identifies institutions that participated in both the CFSR and NSSE.  
 - **data/cfsr_megafile_codebook.csv** Original codebook for CFSR.  
 
 **data/fire_data.rds** stores a list containing the following (open with e.g. `list2env(readRDS("data/fire_data.rds"), envir = .GlobalEnv)`):
  - **fire_codebook** codebook of CFSR variables.  
  - **fire_survey** student-level responses to CFSR from all survey years (2020-present) and all participating institutions.  
  - **fire_scales** institution-level scales used in calculating *Student Perceptions* component of rankings, as well as that `student_perceptions` variable. To my knowledge these are not provided in single-institution data downloads, and are presented only as letter grades by component in [public reports](https://rankings.fire.org/campus/151351-indiana-university?demo=all&year=2025&csfs=false&neutrality=false&spotlight=yellow); arguably very interpretable but less useful for statistical comparisons that I am interested in!    
  - **fire_insts** table of participation FIRE CFSR institutions with flags for concurrent NSSE administrations. NSSE is typically administered in the late winter or spring; FIRE surveys have been administered roughly in the first half of the year. Notably includes `unitid`.  

## Disclaimers
I am not in any way affiliated with FIRE or College Pulse; I am merely interested in sharing something I found useful as a convenience to other researchers in higher education and maintaining a single source for future and ongoing projects. All data shared here are publicly available in some form or another, but not so compiled. While I took steps to validate the accuracy of the student perception scales by following the pre-registered methodology and attaching the correct `unitid` field, errors may occur. Please let me know if any mistakes can be identified.

Some references to NSSE data appear throughout the R scripts; while these data are largely [publicly available](https://nsse.indiana.edu/support-resources/institution-search/index.html) the files are not yet included here.  