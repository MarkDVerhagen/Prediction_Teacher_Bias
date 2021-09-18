## -- Load Libraries
library(tidyverse)

## Script to merge pupil, test and school information

## -- Read sets
pupils <- readRDS("data/edit/pupils.rds")
cito <- readRDS("data/edit/cito.rds")
school <- readRDS("data/edit/schools.rds")

## -- Merge sets
final_df <- merge.data.frame(pupils, cito, by.x = "p_id", by.y = "p_id") %>%
  merge.data.frame(school, by.x = "s_id", by.y = "s_id") %>%
  filter(!duplicated(p_id))

count_schools <- count(final_df, s_id)
final_df <- final_df %>%
  filter(s_id %in% count_schools$s_id[count_schools$n >= 30])


for (i in names(final_df)) {
  print(i)
  print(sum(is.na(final_df[, i])))
}

saveRDS(final_df, "data/edit/final.rds")