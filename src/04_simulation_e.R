## Simulation for additional random effects for ordered outcome models
## output: 1_250_mop_full_perf.rds

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

## -- Run simulation

n_simul <- 50
cores <- detectCores()
cl <- makeCluster(cores[1] - 3)
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

    ## -- Multilevel models
    forms_mle <- lapply(forms, FUN = function(x) paste0(x, " + (1 | s_id)"))

    ## Ordered - full RE
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

    fit <- lapply(forms_mle_full,
        FUN = function(x) {
            ordinal::clmm(formula(x),
                data = train_df, link = "probit", Hess = T
            )
        }
    )

    mop_perf <- lapply(fit, FUN = function(x) {
        predict_opm(x, test_df, full_re = T)
    })
    mop_perf_num <- lapply(mop_perf, FUN = function(x) {
        mean(x$prediction == test_df$y_cat)
    })

    list(mop_perf_num)
}

Sys.time()
results_df <- as.data.frame(results)

saveRDS(results_df, "data/simulations/1_250_mop_full_perf.rds")