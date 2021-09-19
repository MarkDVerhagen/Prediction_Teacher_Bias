## Simulation for performance of random intercept models versus standard models
## on school level
## output: 1_250_school.rds

packages <- c(
  "tidyverse", "caret", "lme4", "leaps", "reshape2",
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
forms_mle_full <- lapply(forms_mle, FUN = function(x) gsub(" ~ ", " ~ (", x))
forms_mle_full <- lapply(forms_mle_full,
  FUN = function(x) gsub("\\+", " -1| s_id) + (", x)
)
forms_mle_full <- lapply(forms_mle_full,
  FUN = function(x) gsub("cohort_2019  -1\\| s_id", "cohort_2019", x)
)
forms_mle_full <- lapply(forms_mle_full,
  FUN = function(x) gsub("\\( \\(", "\\(", x)
)

## -- Run simulation

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


  ## -- Resample pupils in school with replacement
  schools <- unique(df$s_id)
  new_df <- c()

  for (s in schools) {
    sample_frame <- df %>% filter(s_id == s)
    pupils <- sample(1:dim(sample_frame)[1],
      length(sample_frame$p_id),
      replace = T
    )
    new_pupil <- sample_frame[pupils, ]
    new_df <- rbind(new_df, new_pupil)
  }

  new_df$new_p_id <- 1:dim(new_df)[1]

  train_set <- new_df %>%
    group_by(s_id) %>%
    sample_frac(0.75, replace = F)

  # save train and test p_ids
  test_ids <- new_df$new_p_id[!(new_df$new_p_id %in% train_set$new_p_id)]
  train_ids <- train_set$new_p_id

  stopifnot((length(test_ids) + length(train_ids)) == dim(new_df)[1])

  # make train_df and test_df
  test_df <- new_df %>%
    filter(new_p_id %in% test_ids)
  train_df <- new_df %>%
    filter(new_p_id %in% train_ids)

  fit_full <- ordinal::clmm(formula(forms_mle_full[[3]]),
    data = train_df,
    link = "probit", Hess = T
  )

  fit_none <- MASS::polr(formula(forms_base[[3]]), data = train_df, Hess = T)

  full_predict <- data.frame(
    s_id = test_df$s_id,
    predict = predict_opm(fit_full, test_df, full_re = T)$prediction,
    actual = test_df$y_cat
  )
  none_predict <- data.frame(
    s_id = test_df$s_id, predict = as.numeric(predict(fit_none, test_df)),
    actual = test_df$y_cat
  )

  full_score <- full_predict %>%
    group_by(s_id) %>%
    summarise(mean_level_full = mean(predict))

  none_score <- none_predict %>%
    group_by(s_id) %>%
    summarise(mean_level_none = mean(predict))

  final <- full_score %>%
    left_join(none_score) %>%
    mutate(school_effect = mean_level_full - mean_level_none)

  list(final)
}

results_df <- as.data.frame(results)

saveRDS(results_df, "data/simulations/1_250_school.rds")