## Simulation for performance of Linear and Ordered outcome models (R.1)
## Output: 1_250_lm_lme_op_mop_perf.rds

packages <- c(
  "tidyverse", "caret", "lme4",
  "reshape2", "MASS", "ordinal", "doParallel"
)

lapply(packages, require, character.only = TRUE)

## --- Read Data
data <- readRDS("data/edit/analysis.rds")
source("analysis/functions/analysis_functions.R")

national_tests <- TRUE
use_mean_tests <- TRUE

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

df <- data %>%
  dplyr::select(
    p_id, y, s_id, all_of(tests),
    pupil_gender, cohort, weight_03, weight_12, DEN
  ) %>%
  mutate(
    y_cat = as.factor(y),
    s_id = as.factor(s_id),
    cohort_2019 = ifelse(cohort == "2019", 1, 0),
    pupil_low_ses = ifelse((weight_03 == 1) | (weight_12 == 1), 1, 0)
  ) %>%
  dplyr::select(-y, -cohort, -weight_03, -weight_12) %>%
  filter(rowSums(is.na(data %>% dplyr::select(all_of(tests)))) == 0)

## -- Setup models
controls <- c("cohort_2019")
p_gender <- c("pupil_gender")
p_gender <- c("pupil_gender")
p_ses <- c("pupil_ses")
ability <- mean_tests

## -- Run simulation

n_simul <- 250
cores <- detectCores()
cl <- makeCluster(cores[1] - 2)
registerDoParallel(cl)

Sys.time()
results <- foreach(
  i = 1:n_simul,
  .combine = rbind,
  .packages = c("MASS", "ordinal", "tidyverse")
) %dopar% {
  set.seed(i)

  # make train set
  train_set <- df %>%
    group_by(s_id) %>%
    sample_frac(0.75, replace = F)

  # save train and test p_ids
  test_ids <- df$p_id[!(df$p_id %in% train_set$p_id)]
  train_ids <- train_set$p_id

  stopifnot((length(test_ids) + length(train_ids)) == dim(df)[1])

  # make train_df and test_df
  test_df <- df %>%
    filter(p_id %in% test_ids)
  train_df <- df %>%
    filter(p_id %in% train_ids)

  # run models

  # full sets
  call <- "y_cat ~ cohort_2019 + "
  ivs <- lapply(list(
    paste(tests), c(tests, "pupil_gender"),
    c(tests, "pupil_low_ses")
  ),
  FUN = function(x) paste0(x, collapse = "+")
  )
  forms <- lapply(ivs, FUN = function(x) paste(call, x, collapse = "+"))

  ## -- Non-multilevel models

  # performance
  fit <- lapply(forms, FUN = function(x) {
    lm(formula(x), data = train_df %>% mutate(y_cat = as.numeric(y_cat)))
  })
  lm_perf <- lapply(fit, FUN = function(x) lm_performance(x, test_df))

  ## Ordered outcome

  # performance
  fit <- lapply(forms, FUN = function(x) {
    MASS::polr(formula(x), data = train_df, Hess = T)
  })
  op_perf <- lapply(fit, FUN = function(x) {
    mean(predict(x, test_df) == test_df$y_cat)
  })

  ## -- Multilevel models

  forms_mle <- lapply(forms, FUN = function(x) paste0(x, " + (1 | s_id)"))

  # performance
  fit <- lapply(forms_mle, FUN = function(x) {
    lme4::lmer(formula(x), data = train_df %>%
      mutate(y_cat = as.numeric(y_cat)))
  })
  lme_perf <- lapply(fit, FUN = function(x) lm_performance(x, test_df))

  ## Ordered outcome

  # performance
  fit <- lapply(forms_mle, FUN = function(x) {
    ordinal::clmm(formula(x), data = train_df, link = "probit", Hess = T)
  })
  mop_perf <- lapply(fit, FUN = function(x) predict_opm(x, test_df))

  mop_perf <- lapply(mop_perf, FUN = function(x) {
    mean(x$prediction == test_df$y_cat)
  })

  list(lm_perf, lme_perf, op_perf, mop_perf)
}

results_df <- as.data.frame(results)

saveRDS(results_df, "data/simulations/1_250_lm_lme_op_mop_perf.rds")
