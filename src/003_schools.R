## -- Load libraries
library(tidyverse)

## Script to format and wrangle school information.
## Generates explanatory variables on school denomination,
## neighbourhood deprivation,
## school weight, porportion non-western

## -- Read data

schools_raw <- read.csv("data/raw/schools.csv") %>%
       select(-X) %>%
       filter(!duplicated(school_id))

## -- Recategorize school types into Public, Roman, Protestant,
## Confessional, Alternative, and Other

AB <- c("Algemeen bijzonder", "Antroposofisch")
CON <- c("Islamitisch", "Interconfessioneel", "Evangelisch")
OB <- c("Openbaar")
OT <- c("Samenwerking Opb., RK", "Samenwerking PC, RK")
PC <- c("Protestants-Christelijk")
RE <- c("Reformatorisch", "Gereformeerd vrijgemaakt")
RK <- c("Rooms-Katholiek")


schools_d <- schools_raw %>%
       rename(DEN = DENOMINATIE) %>%
       mutate(DEN = ifelse(DEN %in% AB, "AB",
              ifelse(DEN %in% CON, "CON",
                     ifelse(DEN %in% OB, "OB",
                            ifelse(DEN %in% OT, "OT",
                                   ifelse(DEN %in% PC, "PC",
                                          ifelse(DEN %in% RE, "RE",
                                                 ifelse(DEN %in% RK,
                                                        "RK",
                                                        "Unassigned"
                                                 )
                                          )
                                   )
                            )
                     )
              )
       )) %>%
       mutate(
              DEP = ifelse(KL18 < 6, 1,
                     ifelse(KL18 < 7, 2,
                            ifelse(KL18 < 8, 3, 4)
                     )
              ),
              DEP = ifelse(is.na(DEP), 2.5, DEP),
              SWEIGHT = ifelse(school_weight < 28, 1,
                     ifelse(school_weight < 31, 2, 3)
              ),
              PNW = ifelse(prop_non_western < 0.06, 1,
                     ifelse(prop_non_western < 0.16, 2, 3)
              )
       )

schools_final <- schools_d %>%
       select(-ind_wbi, -a_inw, -a_nw_all) %>%
       rename(s_id = school_id) %>%
       mutate(s_id = as.character(s_id))

saveRDS(schools_final, "data/edit/schools.rds")