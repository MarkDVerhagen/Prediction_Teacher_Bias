## Simulation for full random effects on Ordered outcome models
## output: 1_250_mop_full_perf_complete.rds

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

## --- Subset schools with at least 30 obs

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

  # run complete models

  complete <- paste0(
    "y_cat ~ (cohort_2019) + (mean_Reading -1| s_id) +",
    "(mean_Maths -1| s_id) + (mean_Language -1| s_id) +",
    "(pupil_low_ses -1| s_id) +",
    "(pupil_gender -1| s_id) +",
    "(1 | s_id)"
  )

  fit_complete <- ordinal::clmm(complete, data = train_df, link = "probit", Hess = T)
  mop_perf_complete <- predict_opm(fit_complete, test_df, full_re = T)
  mop_perf_num_complete <- mean(mop_perf_complete$prediction == test_df$y_cat)

  list(mop_perf_num_complete)
}

results_df <- as.data.frame(results)

saveRDS(results_df, "data/simulations/1_250_mop_full_perf_complete.rds")
