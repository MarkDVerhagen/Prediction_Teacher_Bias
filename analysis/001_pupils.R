## -- Load libraries
library(tidyverse)
library(lubridate)
library(data.table)

## Script to format and wrangle demographic information on pupils.
## Drops duplicates and general cleaning.
## Selects only those pupils who graduated in 2018 or 2019
## Generates required explanatory variables on the pupil level:
## Sex, Parental Education, School identifier

## Read pupil data.

p_df <- readRDS("data/raw/pupil.rds") %>%
  rename(
    s_id = school_id,
    c_id = class_id,
    p_id = pupil_id,
    pupil_date_birth = pupil_month_birth
  ) %>%
  select(-X, -sibling_id) %>%
  filter(!(pupil_grade %in% c(0, 1, 2))) # omit first and second grade

## -- Remove pupils who have a grade 9: 57 obs (grade 9 doesn't exist)

check_g9 <- p_df %>%
  filter(pupil_grade == 9) %>%
  filter(!duplicated(p_id))

p_df <- p_df %>%
  filter(!(p_id %in% check_g9$p_id))

## -- Select relevant subset of pupils for analysis (non duplicates)

p_df_u <- p_df[!duplicated(p_df %>% select(-cohort, -c_id)), ]

## -- Pupils with two birthdates (duplicate ids): < 9 obs

p_dt_u <- data.table(p_df_u)

double_ids <- p_dt_u[,
  .(birth_dates = uniqueN(pupil_date_birth)),
  by = p_id
] %>%
  as.data.frame() %>%
  filter(birth_dates >= 2) %>%
  select(p_id)

## -- Select grade 8 pupils only (final grade before graduation)

pupils_8 <- p_df_u %>%
  filter(pupil_grade == 8)

p_df_u <- p_df_u %>%
  filter(!(p_id %in% double_ids$p_id)) %>%
  filter(p_id %in% pupils_8$p_id)

## -- Generate some extra variables to check with population statistics
## Notably: number of pupils skipping / doubling a grade and average age

## Skipped pupils: go through grades 3 and 8 in more than 6 years
## Doubled pupils: go through grades 3 and 8 in less than 6 years

p_df_u$exp_cohort <- p_df_u$school_year + (8 - p_df_u$pupil_grade)

p_dt_u <- data.table(p_df_u)

p_n_cohorts_1p <- p_dt_u[, .(
  max_grade = max(pupil_grade),
  min_grade = min(pupil_grade),
  max_year = max(school_year),
  min_year = min(school_year)
), by = p_id] %>%
  as.data.frame() %>%
  mutate(
    no_grades = max_grade - min_grade,
    no_years = max_year - min_year
  )

skippers <- p_n_cohorts_1p %>%
  filter(no_grades > no_years)

doublers <- p_n_cohorts_1p %>%
  filter(no_grades < no_years)

## Age in grade 3

p_df_u_age <- p_df_u %>%
  arrange(p_id, school_year) %>%
  filter(!duplicated(p_id)) %>%
  mutate(
    date_g3 = as.Date(paste0(school_year, "-09-01")),
    pupil_date_birth = as.Date(pupil_date_birth),
    age_grade = date_g3 - pupil_date_birth,
    age_grade = age_grade - (pupil_grade - 3) * 365
  )

count(p_df_u_final, pupil_gender)
count(p_df_u_final, pupil_inspection_weight)

## -- Only include cohorts up until those finishing in 2019
p_df_u_final <- p_df_u_age %>%
  filter(cohort <= 2019) %>%
  select(
    p_id, c_id, s_id, pupil_gender, pupil_inspection_weight,
    pupil_school_advice, cohort, age_grade
  ) %>%
  filter(
    pupil_gender != "",
    !is.na(pupil_inspection_weight),
    pupil_school_advice != ""
  ) %>%
  mutate(
    p_id = as.character(p_id),
    c_id = as.character(c_id),
    s_id = as.character(s_id),
    pupil_gender = ifelse(pupil_gender == "V", 1, 0),
    weight_03 = ifelse(pupil_inspection_weight == 0.3, 1, 0),
    weight_12 = ifelse(pupil_inspection_weight == 1.2, 1, 0),
    skipped = ifelse(p_id %in% skippers$p_id, 1, 0),
    doubled = ifelse(p_id %in% doublers$p_id, 1, 0)
  ) %>%
  filter(
    !grepl("9_", pupil_school_advice),
    cohort %in% c(2018, 2019)
  )

saveRDS(p_df_u_final, "data/edit/pupils.rds")

## -- Some checks on missingness / representativity
non_gender_ids <- p_df_u_age[p_df_u_age$pupil_gender == "", ]
gender_ids <- p_df_u_age[p_df_u_age$pupil_gender != "", ]
count(non_gender_ids, cohort)
count(gender_ids, cohort)

unique(gender_ids[gender_ids$cohort == 2016, "s_id"])
unique(non_gender_ids[non_gender_ids$cohort == 2016, "s_id"])

## -- Missing school advice data checks
no_advice <- p_df_u_age %>%
  filter(pupil_school_advice == "")