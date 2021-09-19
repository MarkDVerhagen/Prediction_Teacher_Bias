## -- Load Libraries
library(tidyverse)
source("analysis/functions/analysis_functions.R")

# Read data ---------------------------------------------------------------

df <- readRDS("data/edit/final_analysis.rds")

base_call <- paste0(
  "y_cat ~ cohort_2019 + mean_Reading + mean_Maths +",
  "mean_Language + pupil_gender + pupil_low_ses"
)
re_call <- paste(
  "y_cat ~ (1 | s_id) + cohort_2019 + mean_Reading + mean_Maths +",
  "mean_Language + pupil_gender + pupil_low_ses"
)


# Table D.1 ---------------------------------------------------------------
desc_df <- df %>%
  mutate(
    girl = ifelse(pupil_gender == 1, 1, 0),
    boy = ifelse(pupil_gender == 0, 1, 0),
    cohort_2018 = 1 - cohort_2019
  )

labs <- data.frame(
  mean_Maths = "Maths",
  mean_Language = "Language",
  mean_Reading = "Reading",
  girl = "Girl",
  boy = "Boy",
  y_cat = "Assigned Track Level",
  pupil_low_ses = "Low Parental Education",
  SWEIGHT = "School Disadvantage",
  ste_mvs = "School Urbanity",
  cohort_2019 = "Cohort 2018-2019",
  cohort_2018 = "Cohort 2017-2018",
  DEN = "School Denomination"
)

# Table R.1 -----------------------------------------------------------------

lmr_fit <- estimatr::lm_robust(as.formula(base_call),
  clusters = s_id, data = df %>% mutate(y_cat = as.numeric(y_cat))
)
re_fit <- lme4::lmer(re_call, data = df %>% mutate(y_cat = as.numeric(y_cat)))

## Ordered outcome // Full sample
op_fit <- ordinal::clm(base_call, data = df, Hess = T)
mop_fit <- ordinal::clmm(re_call, data = df, link = "probit", Hess = T)
opr_fit <- op_fit
opr_fit$vcov <- sandwich::vcovCL(opr_fit, df$s_id)

texreg::texreg(list(lmr_fit, re_fit, opr_fit, mop_fit),
  include.ci = F, center = T,
  file = "tex/tables/table_res_1_new.tex"
)

save(lm_fit, re_fit, op_fit, mop_fit, file = "data/results/model_results.rda")

# Table R.2 -----------------------------------------------------------------

op_fit_s_den <- ordinal::clm(paste0(base_call, " + DEN"),
  data = df, Hess = T
)
op_fit_s_sw <- ordinal::clm(paste0(base_call, " + SWEIGHT"),
  data = df, Hess = T
)
op_fit_s_urb <- ordinal::clm(paste0(base_call, " + ste_mvs"),
  data = df, Hess = T
)
op_fit_s_all <- ordinal::clm(paste0(base_call, " + DEN + SWEIGHT + ste_mvs"),
  data = df, Hess = T
)

ability_call <- paste0(
  "y_cat ~ cohort_2019 + mean_Reading + mean_Maths +",
  "mean_Language"
)

mop_fit_s_sex <- ordinal::clmm(paste0(ability_call, "+ (pupil_gender | s_id)"),
  data = df, link = "probit", Hess = T
)
mop_fit_s_ses <- ordinal::clmm(paste0(ability_call, "+ (pupil_low_ses | s_id)"),
  data = df, link = "probit", Hess = T
)

save(mop_fit_s_sex, mop_fit_s_ses, file = "data/results/mop_s_results.rda")

texreg::texreg(list(
  op_fit_s_den,
  op_fit_s_sw,
  op_fit_s_urb,
  op_fit_s_all
),
file = "tex/tables/table_res_2_new.tex"
)

texreg::texreg(list(
  op_fit_s_den %>% norm_op_reg(), op_fit_s_sw %>% norm_op_reg(),
  op_fit_s_urb %>% norm_op_reg(), op_fit_s_all %>% norm_op_reg()
),
file = "tex/tables/table_res_full.tex"
)

# Table R.3 ---------------------------------------------------------------

set.seed(1)
train <- df %>%
  group_by(s_id) %>%
  sample_frac(0.75)

# generate test set, sampled by school
test <- df %>%
  filter(!(p_id %in% train$p_id)) %>%
  dplyr::select(-p_id)
train$p_id <- NULL

delta_op_g <- as.numeric(predict(fit_op_p[[1]], test)) -
  as.numeric(test$y_cat)
delta_op_sex <- as.numeric(predict(fit_op_p[[2]], test)) -
  as.numeric(test$y_cat)
delta_op_ses <- as.numeric(predict(fit_op_p[[3]], test)) -
  as.numeric(test$y_cat)

delta_mop_g <- as.numeric(predict_opm(fit_mop_p[[1]], test)$prediction) -
  as.numeric(test$y_cat)
delta_mop_sex <- as.numeric(predict_opm(fit_mop_p[[2]], test)$prediction) -
  as.numeric(test$y_cat)
delta_mop_ses <- as.numeric(predict_opm(fit_mop_p[[3]], test)$prediction) -
  as.numeric(test$y_cat)

df_plot <- data.frame(
  grades = delta_op_g, sex = delta_op_sex, ses = delta_op_ses,
  mop_grades = delta_mop_g, mop_sex = delta_mop_sex, mop_ses = delta_mop_ses
)

table_results <- count(df_plot, grades) %>%
  rename(delta = grades) %>%
  left_join(count(df_plot, sex) %>%
    rename(delta = sex), by = "delta") %>%
  left_join(count(df_plot, ses) %>%
    rename(delta = ses), by = "delta") %>%
  left_join(count(df_plot, mop_grades) %>%
    rename(delta = mop_grades), by = "delta") %>%
  left_join(count(df_plot, mop_sex) %>%
    rename(delta = mop_sex), by = "delta") %>%
  left_join(count(df_plot, mop_ses) %>%
    rename(delta = mop_ses), by = "delta")

names(table_results) <- c(
  "Delta", "Ability", "Ability + Sex", "Ability + Ses",
  "Ability + School effects", "Ability + School effects + Sex",
  "Ability + School effects + Ses"
)

table_results[is.na(table_results)] <- 0

write.table(table_results, "tables/table_res_3.txt")
