## Simulation for performance when estimating and predicting on subsets
## output: 1_250_mop_cross_within.rds

packages <- c(
  "tidyverse", "caret", "stargazer", "lme4", "leaps", "reshape2",
  "MASS", "ordinal", "doParallel"
)

lapply(packages, require, character.only = TRUE)

## --- Read Data
data <- readRDS("data/edit/analysis.rds")
source("analysis/functions/analysis_functions.R")

national_tests <- TRUE
use_mean_tests <- TRUE

## --- Subset schools with at least 30 obs

if (national_tests) {
  tests <- names(data)[grepl(
    "^Reading|^Maths|^Language",
    names(data)
  )]
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

# full sets
call <- "y_cat ~ cohort_2019 + "
ivs <- lapply(list(
  paste(tests), c(tests, "pupil_gender"),
  c(tests, "pupil_low_ses")
),
FUN = function(x) paste0(x, collapse = "+")
)

forms_base <- lapply(ivs, FUN = function(x) paste(call, x, collapse = "+"))
forms_mle <- lapply(forms_base, FUN = function(x) paste0(x, " + (1 | s_id)"))

## -- Run simulation
resample <- T
n_simul <- 250
cores <- detectCores()
cl <- makeCluster(cores[1] - 2)
registerDoParallel(cl)

Sys.time()
results <- foreach(
  i = 1:n_simul,
  .combine = rbind,
  .packages = c("MASS", "ordinal", "tidyverse", "caret")
) %dopar% {
  set.seed(i)

  new_df <- df

  if (resample) {
    new_df$math_two_decile <- ceiling(new_df$mean_Maths * 5)
    target_dist <- count(new_df %>% filter(pupil_low_ses == 0), math_two_decile)
    new_df$sample_id <- 1:dim(new_df)[1]
    sample_frame <- c(
      sample(new_df$sample_id[(new_df$math_two_decile == 1) &
        (new_df$pupil_low_ses == 1)], target_dist$n[1], replace = T),
      sample(new_df$sample_id[(new_df$math_two_decile == 2) &
        (new_df$pupil_low_ses == 1)], target_dist$n[2], replace = T),
      sample(new_df$sample_id[(new_df$math_two_decile == 3) &
        (new_df$pupil_low_ses == 1)], target_dist$n[3], replace = T),
      sample(new_df$sample_id[(new_df$math_two_decile == 4) &
        (new_df$pupil_low_ses == 1)], target_dist$n[4], replace = T),
      sample(new_df$sample_id[(new_df$math_two_decile == 5) &
        (new_df$pupil_low_ses == 1)], target_dist$n[5], replace = T)
    )

    low_ses_df <- new_df[sample_frame, ]
    new_df <- rbind(new_df %>% filter(pupil_low_ses == 0), low_ses_df)
  }


  # generate new p_ids for the resampled frame
  new_df$p_id <- 1:dim(new_df)[1]

  # make sets for sex
  train_set_sex <- new_df %>%
    group_by(s_id) %>%
    sample_frac(0.75, replace = F)

  # save train and test p_ids
  test_ids_sex <- new_df$p_id[!(new_df$p_id %in% train_set_sex$p_id)]
  train_ids_sex <- train_set_sex$p_id


  # make train_df and test_df
  test_df_sex <- new_df %>%
    filter(p_id %in% test_ids_sex)

  train_df_sex <- new_df %>%
    filter(p_id %in% train_ids_sex)

  # make sets for ses
  no_low_ses_train <- train_df_sex %>%
    group_by(s_id) %>%
    summarise(low_ses = sum(pupil_low_ses)) %>%
    filter(low_ses == 0)

  train_df_ses <- train_df_sex %>%
    filter(!(s_id %in% no_low_ses_train$s_id))

  test_df_ses <- test_df_sex %>%
    filter(s_id %in% train_df_ses$s_id)

  fit_mop_sex_0_train <- ordinal::clmm(formula(forms_mle[[1]]),
    data = train_df_sex %>% filter(pupil_gender == 0), link = "probit", Hess = T
  )
  fit_mop_sex_1_train <- ordinal::clmm(formula(forms_mle[[1]]),
    data = train_df_sex %>% filter(pupil_gender == 1), link = "probit", Hess = T
  )

  fit_mop_ses_0_train <- ordinal::clmm(formula(forms_mle[[1]]),
    data = train_df_ses %>% filter(pupil_gender == 0), link = "probit", Hess = T
  )
  fit_mop_ses_1_train <- ordinal::clmm(formula(forms_mle[[1]]),
    data = train_df_ses %>% filter(pupil_gender == 1), link = "probit", Hess = T
  )

  ses_00 <- table(predict_opm(fit_mop_ses_0_train, test_df_ses %>%
    filter(pupil_low_ses == 0))$prediction)
  ses_10 <- table(predict_opm(fit_mop_ses_1_train, test_df_ses %>%
    filter(pupil_low_ses == 0))$prediction)

  ses_01 <- table(predict_opm(fit_mop_ses_0_train, test_df_ses %>%
    filter(pupil_low_ses == 1))$prediction)
  ses_11 <- table(predict_opm(fit_mop_ses_1_train, test_df_ses %>%
    filter(pupil_low_ses == 1))$prediction)

  sex_00 <- table(predict_opm(fit_mop_sex_0_train, test_df_sex %>%
    filter(pupil_gender == 0))$prediction)
  sex_10 <- table(predict_opm(fit_mop_sex_1_full, test_df_sex %>%
    filter(pupil_gender == 0))$prediction)

  sex_01 <- table(predict_opm(fit_mop_sex_0_full, test_df_sex %>%
    filter(pupil_gender == 1))$prediction)
  sex_11 <- table(predict_opm(fit_mop_sex_1_ttrain, test_df_sex %>%
    filter(pupil_gender == 1))$prediction)

  list(ses_00, ses_10, ses_01, ses_11, ses_00, ses_10, ses_01, ses_11)
}

results_df <- as.data.frame(results)

if (resample) {
  saveRDS(results_df, "data/simulations/1_250_cross_within_resample_new.rds")
} else {
  saveRDS(results_df, "data/simulations/1_250_cross_within.rds")
}