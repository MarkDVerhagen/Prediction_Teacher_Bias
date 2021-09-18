## -- Load Libraries
library(tidyverse)

## Script to generate outcome variable from the data

## -- Read data
data <- readRDS("data/edit/final.rds")

## -- Define y variable
level_5 <- c("1_vwo")
level_4 <- c("2_havo/vwo")
level_3 <- c("3-havo")
level_2 <- c("4_havo/v-t", "5_vmbo", "6_vmbo-t")
level_1 <- c("7_vmbo lager", "8_PrO")

df <- data %>%
  mutate(y = pupil_school_advice) %>%
  mutate(y = ifelse(y %in% level_5, 5,
    ifelse(y %in% level_4, 4,
      ifelse(y %in% level_3, 3,
        ifelse(y %in% level_2, 2,
          ifelse(y %in% level_1, 1, NA)
        )
      )
    )
  ))

## Check descriptives ofoutcomes
desc_table <- df %>%
  group_by(y) %>%
  summarise_all(funs(mean(., na.rm = T)))

saveRDS(df, "data/edit/analysis.rds")