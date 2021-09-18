## -- Load libraries
library(tidyverse)
library(lubridate)
library(tiger)
library(data.table)

## Script to format and wrangle test information on pupils.
## Merges together multiple tests taken on the same day
## Selects only those test scores from pupils graduating in 2018 or 2019
## Selects only tests done in the final two grades (7 and 8)
## Further uniform test scores using the available sample
## Generate mean scores for Maths, Reading and Language

## Read data
source("analysis/functions/cito_clean_functions.r")

pupils <- readRDS("data/edit/pupils.rds")

cito_raw <- readRDS("data/raw/cito.rds") %>%
  select(-X) %>%
  rename(
    p_id = pupil_id,
    c_id = class_id
  ) %>%
  mutate(
    p_id = as.character(p_id),
    c_id = as.character(c_id),
    date_taken = as.Date(date_taken)
  )

# Generate cross walk from class to school
c_s_cw <- pupils[, c("c_id", "s_id")] %>%
  filter(!duplicated(c_id, s_id)) %>%
  filter(!duplicated(c_id))

cito <- cito_raw %>%
  left_join(c_s_cw)

## Subset to only include relevant test scores and check diagnostics

cito_p <- cito %>%
  filter(p_id %in% pupils$p_id)

cito_sub <- cito_p %>%
  mutate(time_taken = paste0(semester, class_year)) %>%
  filter(time_taken %in% c("B7", "M7", "E7", "B8", "M8", "E8")) %>%
  mutate(month = substr(date_taken, 6, 7))

cito_sub_final <- cito_sub %>%
  filter(!((month %in% c("08", "09")) & (semester == "M")))

ids <- grades_missing_report(pupils, cito_sub_final,
  cito = c("RW", "TBL", "SP", "DMT"),
  return = "nat"
)

cito_full <- cito_sub_final

## Re-uniform scores
cito_full <- cito_full %>%
  group_by(cito_subject, class_year, semester) %>%
  mutate(percentile_score = tiger::to.uniform(percentile_score))


## Mean scores together when they are on the same date and same subject
cito_full_dt <- data.table(cito_full)

cito_3tests <- cito_full_dt[,
  .(percentile_score = mean(percentile_score, na.rm = T)),
  by = c(
    "p_id", "date_taken", "cito_taken_test", "national_subject",
    "school_year", "semester", "class_year"
  )
] %>%
  as.data.frame()

saveRDS(cito_3tests, "data/edit/cito_3tests_clean.rds")

## Aggregate to yearly scores
cito_3tests_year <- cito_3tests %>%
  group_by(p_id, class_year, national_subject) %>%
  summarise(pscore = mean(percentile_score, na.rm = T)) %>%
  ungroup()

## -- Pivot wide
cito_wide_3 <- cito_3tests_year %>%
  pivot_wider(
    id_cols = c("p_id"),
    names_from = c("national_subject", "class_year"),
    values_from = c("pscore")
  )

cito_full <- cito_wide_3

cito_full <- cito_full %>%
  mutate(
    mean_Reading = rowMeans(cito_full %>% select(Reading_7, Reading_8),
      na.rm = T
    ),
    mean_Maths = rowMeans(cito_full %>% select(Maths_7, Maths_8), na.rm = T),
    mean_Language = rowMeans(cito_full %>% select(Language_7, Language_8),
      na.rm = T
    )
  )

saveRDS(cito_full, "data/edit/cito.rds")