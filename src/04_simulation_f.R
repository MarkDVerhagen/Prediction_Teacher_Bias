## Simulation for Linear outcome and Ordered outcome models (Figure 2)

packages <- c("tidyverse", "caret", "lme4",
              "reshape2", "patchwork", "cowplot",
              "MASS", "ordinal", "doParallel", "randomForest")

lapply(packages, require, character.only = TRUE)

## --- Read Data
data <- readRDS("data/edit/analysis.rds")
source("analysis/functions/analysis_functions.R")

national_tests <- TRUE
use_mean_tests <- TRUE

## --- Subset schools with at least 30 obs

if (national_tests) {
  tests <- names(data)[grepl("^Reading|^Maths|^Language", names(data))]
  mean_tests <- names(data)[grepl("mean_Reading|mean_Maths|mean_Language", names(data))]
} else {
  tests <- names(data)[grepl("^RW|^TBL|^DMT|^SP", names(data))]
  mean_tests <- names(data)[grepl("mean_RW|mean_TBL|mean_DMT|mean_SP", names(data))]
}

if (use_mean_tests) {
  tests <- mean_tests
}

df <- data %>%
  dplyr::select(p_id, y, s_id, all_of(tests),
                pupil_gender, cohort, weight_03, weight_12, DEN) %>%
  mutate(y_cat = as.factor(y),
         s_id = as.factor(s_id),
         cohort_2019 = ifelse(cohort == "2019", 1, 0),
         pupil_low_ses = ifelse((weight_03 == 1) | (weight_12 == 1), 1, 0)) %>%
  dplyr::select(-y, -cohort, -weight_03, -weight_12) %>%
  filter(rowSums(is.na(data %>% dplyr::select(all_of(tests)))) == 0)

## -- Setup models
controls <- c("cohort_2019")
p_gender <- c("pupil_gender")
p_gender <- c("pupil_gender")
p_ses <- c("pupil_ses")
ability <- mean_tests
schools <- c("DEN")

## -- Run simulation

n_simul <- 250
cores <- detectCores()
cl <- makeCluster(cores[1] - 2)
registerDoParallel(cl)

Sys.time()
results <- foreach(i = 1 : n_simul, .combine = rbind, .packages = c("MASS", "ordinal", "tidyverse")) %dopar% {
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
  form_op <- "y_cat ~ cohort_2019 +  mean_Reading+mean_Maths+mean_Language+pupil_low_ses+pupil_gender"
  form_mop <- "y_cat ~ (1 | s_id) + cohort_2019 +  mean_Reading+mean_Maths+mean_Language+pupil_low_ses+pupil_gender"
  
  ## -- Non-multilevel models
  
  fit <- MASS::polr(form_op, data = train_df, Hess = T)
  op_perf <- mean(predict(fit, test_df) == test_df$y_cat)
  
  ## -- Multilevel models
  
  fit <- ordinal::clmm(form_mop, data = train_df, link="probit", Hess = T)
  mop_perf <- predict_opm(fit, test_df)
  mop_perf_num <- mean(mop_perf$prediction == test_df$y_cat)
  
  list(op_perf, mop_perf_num)
}

results_df <- as.data.frame(results)

saveRDS(results_df, "data/simulations/1_250_full_pupil_op_mop.rds")


## OLD CODE
# 
# g_opm_s <- results[, 4]
# for (i in 1 : 100) {
#   new_df <- as.data.frame(g_opm_s[i])
#   names(new_df) <- c("s_id", "performance", "n")
#   if (i == 1) {
#     eval_df1 <- new_df %>%
#       dplyr::select(s_id, n, performance)
#   } else {
#     new_df[, as.character(i)] <- new_df$performance
#     eval_df1 <- eval_df1 %>%
#       left_join(new_df %>%
#                   dplyr::select(-n, -performance), by = "s_id")
#   }
# }
# 
# 
# g_gender_opm_s <- results[, 5]
# for (i in 1 : 100) {
#   new_df <- as.data.frame(g_gender_opm_s[i])
#   names(new_df) <- c("s_id", "performance", "n")
#   if (i == 1) {
#     eval_df2 <- new_df %>%
#       dplyr::select(s_id, n, performance)
#   } else {
#     new_df[, as.character(i)] <- new_df$performance
#     eval_df2 <- eval_df2 %>%
#       left_join(new_df %>%
#                   dplyr::select(-n, -performance), by = "s_id")
#   }
# }
# 
# g_gender_ses_opm_s <- results[, 6]
# for (i in 1 : 100) {
#   new_df <- as.data.frame(g_gender_ses_opm_s[i])
#   names(new_df) <- c("s_id", "performance", "n")
#   if (i == 1) {
#     eval_df3 <- new_df %>%
#       dplyr::select(s_id, n, performance)
#   } else {
#     new_df[, as.character(i)] <- new_df$performance
#     eval_df3 <- eval_df3 %>%
#       left_join(new_df %>%
#                   dplyr::select(-n, -performance), by = "s_id")
#   }
# }
# 
# improvements <- data.frame("s_id" = eval_df1$s_id, "n" = eval_df1$n,
#                           "grades_only" = rowMeans(eval_df1[, 3: 102]),
#                           "grades_gender" = rowMeans(eval_df2[, 3: 102]),
#                           "grades_gender_ses" = rowMeans(eval_df3[, 3: 102]))
# 
# improvements_melt <- improvements %>%
#   dplyr::select(-n) %>%
#   melt(id.vars = "s_id")
# 
# ggplot(improvements_melt) + geom_density(aes(x = value, fill = variable), alpha=0.6) +
#   scale_fill_brewer(palette = "Set2") + theme_bw() +
#   labs(title = "Distribution of school-level performance of predictions",
#        x = "Proportion correctly predicted", y = "Density")
# 
# improvements_s <- improvements %>%
#   left_join(schools)
# improv <- improvements_s
# 
# performance_by_s <- function(improv, var = "DEN") {
#   improv$var <- improv[, c(var)]
#   return(improv %>%
#            mutate(n = 1) %>%
#            mutate(var = as.factor(var)) %>%
#            group_by(var) %>%
#            summarise(grades = mean(grades_only),
#                      grades_g = mean(grades_gender),
#                      mean(grades_gender_ses),
#                      n = sum(n)))
# }
# head(schools)
# performance_by_s(improvements_s, "DEN")
# performance_by_s(improvements_s, var = "SWEIGHT")
# performance_by_s(improvements_s, var = "KL18")
# performance_by_s(improvements_s, var = "ste_mvs")
#   
# 
# 
# mean(round(improvements$grades_only, 2) < round(improvements$grades_gender_ses, 2))
# mean(round(improvements$grades_only, 2) == round(improvements$grades_gender_ses, 2))
# mean(round(improvements$grades_only, 2) > round(improvements$grades_gender_ses, 2))
# 
# mean(improvements$grades_only < improvements$grades_gender)
# mean(improvements$grades_only == improvements$grades_gender)
# mean(improvements$grades_only > improvements$grades_gender)
# 
# results_df <- as.data.frame(results)
# 
# names(results_df) <- c("opm_g", "opm_g_gender", "opm_g_gender_ses")
# 
# mean(unlist(results_df$opm_g))
# sd(unlist(results_df$opm_g))
# 
# mean(unlist(results_df$opm_g_gender))
# sd(unlist(results_df$opm_g_gender))
# 
# mean(unlist(results_df$opm_g_gender_ses))
# sd(unlist(results_df$opm_g_gender_ses))
# 
# 
# Sys.time()
# saveRDS(results_df, "data/results/200915_op_full_simul_100.rds")
