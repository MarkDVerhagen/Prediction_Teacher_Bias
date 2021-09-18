### Analysis file for Non-linearities in Teacher Bias
## Author: Mark Verhagen

## -- Load Packages
packages <- c(
  "tidyverse", "reshape2", "randomForest", "caret", "xgboost",
  "MASS", "ordinal", "lme4", "gtools", "texreg", "fastDummies"
)
lapply(packages, require, character.only = TRUE)

## -- Read Data
data <- readRDS("data/edit/analysis.rds")
source("analysis/functions/analysis_functions.R")

national_tests <- TRUE
use_mean_tests <- TRUE

## -- Choose what classification of tests to use
if (national_tests) {
  tests <- names(data)[grepl("^Reading|^Maths|^Language", names(data))]
  mean_tests <- names(data)[grepl(
    "mean_Reading|mean_Maths|mean_Language",
    names(data)
  )]
} else {
  tests <- names(data)[grepl("^RW|^TBL|^DMT|^SP", names(data))]
  mean_tests <- names(data)[grepl(
    "mean_RW|mean_TBL|mean_DMT|mean_SP",
    names(data)
  )]
}

if (use_mean_tests) {
  tests <- mean_tests
}

## -- Generate analysis-ready dataset with all relevant variables
df <- data %>%
  dplyr::select(
    p_id, y, s_id, all_of(tests),
    pupil_gender, cohort, weight_03, weight_12, DEN,
    ste_mvs, SWEIGHT, PNW
  ) %>%
  mutate(
    y_cat = as.factor(y),
    s_id = as.factor(s_id),
    cohort_2019 = ifelse(cohort == "2019", 1, 0),
    pupil_low_ses = ifelse((weight_03 == 1) | (weight_12 == 1), 1, 0)
  ) %>%
  dplyr::select(-y, -cohort, -weight_03, -weight_12) %>%
  filter(rowSums(is.na(data %>%
    dplyr::select(all_of(tests)))) == 0) %>%
  mutate(
    DEN = gsub("CON|PC|RE|RK", "Christian", DEN),
    DEN = gsub("OB", "Public", DEN),
    DEN = gsub("AB|OT", "Other", DEN)
  ) %>%
  mutate(
    DEN = as.factor(DEN),
    PNW = as.factor(PNW),
    ste_mvs = as.factor(ste_mvs),
    SWEIGHT = as.factor(SWEIGHT)
  )

df %>% saveRDS("data/edit/final_analysis.rds")

set.seed(1704)

## -- Generate a train and test split for initial performance evaluation

# generate training set, sampled by school
train_df <- df %>%
  group_by(s_id) %>%
  sample_frac(0.75)

# generate test set, sampled by school
test_df <- df %>%
  filter(!(p_id %in% train_df$p_id)) %>%
  dplyr::select(-p_id)
train_df$p_id <- NULL

## -- Setup models
controls <- c("cohort_2019")
p_gender <- c("pupil_gender")
p_ses <- c("pupil_ses")
ability <- mean_tests

## -- Run models

# full sets
call <- "y_cat ~ cohort_2019 + "

ivs <- lapply(list(
  paste(tests), c(tests, "pupil_gender"),
  c(tests, "pupil_low_ses")
),
FUN = function(x) paste0(x, collapse = "+")
)
forms_base <- lapply(ivs, FUN = function(x) paste(call, x, collapse = "+"))

## -- Non-multilevel models

## -- Linear outcome on full sample

lm_fit <- lapply(forms_base,
  FUN = function(x) {
    lm(formula(x),
      data = df %>% mutate(y_cat = as.numeric(y_cat))
    )
  }
)

lm_coefs <- lapply(lm_fit, FUN = function(x) summary(x)$coefficients)

## Linear outcome on train sample + performance on test sample

fit_lm_p <- lapply(forms_base,
  FUN = function(x) {
    lm(formula(x),
      data = train_df %>% mutate(y_cat = as.numeric(y_cat))
    )
  }
)

lm_perf <- lapply(fit_lm_p, FUN = function(x) lm_performance(x, test_df))

results <- data.frame(
  y_hat = predict(fit_lm_p[[3]], test_df),
  y_cat_hat = round(predict(fit_lm_p[[3]], test_df)),
  real = test_df$y_cat
)

## -- Ordered outcome // Full sample

fit_op <- lapply(forms_base, FUN = function(x) {
  ordinal::clm(formula(x),
    data = df, Hess = T
  )
})
op_coefs <- lapply(fit_op, FUN = function(x) summary(x)$coefficients)

## -- Ordered outcome on train sample + performance on test sample

fit_op_p <- lapply(forms_base,
  FUN = function(x) {
    MASS::polr(formula(x),
      data = train_df, Hess = T, method = "probit"
    )
  }
)

op_perf <- lapply(fit_op_p,
  FUN = function(x) mean(predict(x, test_df) == test_df$y_cat)
)
fit_op_cuts <- lapply(fit_op_p, FUN = function(x) ap_cuts(x))

## School level analysis, including school explanatory variable to model

call_s <- "y_cat ~ 1 + cohort_2019 + "
ivs_s <- lapply(list(
  c(tests, "DEN"), c(tests, "ste_mvs"), c(tests, "SWEIGHT"),
  c(tests, "DEN", "PNW", "ste_mvs", "SWEIGHT")
),
FUN = function(x) paste0(x, collapse = "+")
)
forms_s <- lapply(ivs_s, FUN = function(x) paste0(call_s, x))

fit_op_s <- lapply(forms_s, FUN = function(x) {
  MASS::polr(formula(x),
    data = df, Hess = T, method = "probit"
  )
})

op_s_coefs <- lapply(fit_op_s, FUN = function(x) summary(x)$coefficients)
fit_op_p_s <- lapply(forms_s, FUN = function(x) {
  MASS::polr(formula(x),
    data = train_df, Hess = T, method = "probit"
  )
})

op_perf_s <- lapply(fit_op_p_s,
  FUN = function(x) mean(predict(x, test_df) == test_df$y_cat)
)
fit_op_cuts_s <- lapply(fit_op_p_s, FUN = function(x) ap_cuts(x))

## -- Multilevel models

## -- Linear outcome // Full sample

forms_mle <- lapply(forms_base, FUN = function(x) paste0(x, " + (1 | s_id)"))
lme_fit <- lapply(forms_mle,
  FUN = function(x) {
    lme4::lmer(formula(x),
      data = df %>% mutate(y_cat = as.numeric(y_cat))
    )
  }
)
lme_coefs <- lapply(lme_fit, FUN = function(x) summary(x)$coefficients)

save(forms_mle, lme_fit, lme_coefs, file = "data/results/lme_full.rda")

## -- Linear outcome // Train sample + performance

fit_lme_p <- lapply(forms_mle, FUN = function(x) {
  lme4::lmer(formula(x),
    data = train_df %>% mutate(y_cat = as.numeric(y_cat))
  )
})
lme_perf <- lapply(fit_lme_p, FUN = function(x) lm_performance(x, test_df))

## -- Ordered outcome // Full sample

fit_mop <- lapply(forms_mle,
  FUN = function(x) {
    ordinal::clmm(formula(x),
      data = df, link = "probit", Hess = T
    )
  }
)
mop_coefs <- lapply(fit, FUN = function(x) summary(x)$coefficients)

## -- Ordered outcome // Train sample + performance
fit_mop_p <- lapply(forms_mle,
  FUN = function(x) {
    ordinal::clmm(formula(x),
      data = train_df, link = "probit", Hess = T
    )
  }
)
mop_perf <- lapply(fit_mop_p, FUN = function(x) {
  mean(predict_opm(x, test_df)$prediction == test_df$y_cat)
})

mop_cuts <- lapply(fit_mop_p,
  FUN = function(x) predict_opm(x, test_df)$cuts
)
mop_aps <- lapply(fit_mop_p,
  FUN = function(x) predict_opm(x, test_df)$ap
)

## -- Ordered outcome with full random coefficients (on all coefficients)

forms_mle_full <- lapply(forms_mle,
  FUN = function(x) gsub(" ~ ", " ~ (", x)
)
forms_mle_full <- lapply(forms_mle_full,
  FUN = function(x) gsub("\\+", " -1| s_id) + (", x)
)
forms_mle_full <- lapply(forms_mle_full,
  FUN = function(x) gsub("cohort_2019  -1\\| s_id", "cohort_2019", x)
)
forms_mle_full <- lapply(forms_mle_full,
  FUN = function(x) gsub("\\( \\(", "\\(", x)
)

fit_full <- lapply(forms_mle_full, FUN = function(x) {
  ordinal::clmm(formula(x),
    data = train_df, link = "probit", Hess = T
  )
})

mop_full_perf <- lapply(fit_full,
  FUN = function(x) predict_opm(x, test_df, full_re = T)
)
mop_full_cuts <- lapply(fit_full,
  FUN = function(x) predict_opm(x, test_df, full_re = T)$cuts
)
mop_full_perf_num <- lapply(mop_full_perf,
  FUN = function(x) mean(x$prediction == test_df$y_cat)
)

save(lm_coefs, lm_perf, lme_coefs, lme_perf,
  op_coefs, op_perf, mop_coefs, mop_perf,
  fit_lm_p, fit_lme_p, fit_mop_p, fit_op_p,
  fit_op_s, op_s_coefs,
  mop_full_perf, test_df, train_df, df,
  file = "analysis/coefs_perfs.rda"
)