## -- Analysis functions


lm_performance <- function(mod, test) {
  ### Calculate linear model performance
  ### Round predicted tracks and assign
  ### all value < 0.5 to the lowest track
  lm_predicts <- round(predict(mod, test))
  lm_predicts <- ifelse(lm_predicts < 1, 1,
    ifelse(lm_predicts > 5, 5, lm_predicts)
  )
  return(mean(lm_predicts == as.numeric(test_df$y_cat)))
}

predict_opm <- function(mod, test, full_re = F) {
  ### Make predictions for OPM model
  ### when using random effects

  # Obtain fixed effect coefficients
  coefs <- mod$beta
  fixed_df <- data.frame(
    y_hat = as.matrix(test[, names(coefs)]) %*% coefs,
    s_id = test$s_id
  )

  # obtain random effects and put into dataframe
  ranefs <- as.data.frame(ordinal::ranef(mod))
  ran_coefs <- gsub("s_id.", "", names(ranefs))
  ran_coefs <- gsub("\\.", "", ran_coefs)
  ranefs$s_id <- rownames(ranefs)

  if (full_re) { # dependent on full random effects (slopes + intercept)
    slope_coefs <- ran_coefs[!grepl("Intercept", ran_coefs)]
    ranef_obs <- data.frame(int = 1, s_id = test_df$s_id)
    ranef_obs[, slope_coefs] <- test_df[, slope_coefs]

    names(ranefs) <- gsub("s_id\\.\\.|s_id\\.|X\\.|\\.", "", names(ranefs))
    ranefs <- ranefs[, c("Intercept", slope_coefs, "s_id")]

    # generate dataframe with rows per school with posterior estimates
    ranef_full <- ranef_obs %>%
      left_join(ranefs, by = "s_id") %>%
      dplyr::select(-s_id)

    # inner product of coefficients and observed values for random variables
    n_col <- dim(ranef_full)[2]
    ranef_part <- rowSums(ranef_full[, 1:(n_col / 2)] *
      ranef_full[, (n_col / 2 + 1):n_col])

    full_df <- data.frame(full_y = fixed_df$y_hat + ranef_part)
  } else {
    full_df <- fixed_df %>%
      left_join(ranefs, by = "s_id") %>%
      rename(re_school = X.Intercept.) %>%
      mutate(re_school = ifelse(is.na(re_school), 0, re_school)) %>%
      mutate(full_y = y_hat + re_school)
  }

  linspace <- full_df$full_y

  zetas <- mod$alpha

  # calculate probabilities per threshold
  df <- data.frame(
    p1 = pnorm(zetas[1] - linspace, 0, 1),
    p2 = pnorm(zetas[2] - linspace, 0, 1) - pnorm(zetas[1] - linspace, 0, 1),
    p3 = pnorm(zetas[3] - linspace, 0, 1) - pnorm(zetas[2] - linspace, 0, 1),
    p4 = pnorm(zetas[4] - linspace, 0, 1) - pnorm(zetas[3] - linspace, 0, 1),
    p5 = 1 - pnorm(zetas[4] - linspace, 0, 1)
  )

  # select maximum probability
  final_df <- data.frame(
    fixed = linspace,
    prediction = apply(df, 1, function(x) which.max(x))
  )

  # get first cutoff point based on fixed effect to extract implied threshold
  mop_cuts <- final_df %>%
    arrange(fixed) %>%
    filter(
      !duplicated(prediction),
      prediction != 1
    )

  ap_mop <- data.frame(actuals = test$y_cat, predicts = final_df$fixed) %>%
    mutate(correct = ifelse(final_df$prediction == test$y_cat,
      "Correct", "Wrong"
    ))

  result <- list(
    final_df$fixed, final_df$prediction, mop_cuts$fixed,
    ap_mop
  )

  names(result) <- c("y_hat", "prediction", "cuts", "ap")
  return(result)
}

norm_op_reg <- function(fit, var_regex = "Maths") {
  norm_coef <- fit$coefficients[grepl(var_regex, names(fit$coefficients))]
  fit$coefficients <- fit$coefficients / norm_coef
  fit$vcov <- fit$vcov / norm_coef^2
  return(fit)
}

return_opm <- function(opm, test) {
  print(summary(opm))
  print(paste0(
    "Performance: ",
    mean(round(predict(opm, test) == test$y_cat, 2))
  ))
}

generate_opm_ranef_df_s <- function(object_cclm) {
  opm_ranef <- data.frame(
    estimate = ordinal::ranef(object_cclm)$s_id,
    lb = ordinal::ranef(object_cclm) - 2 * sqrt(ordinal::condVar(object_cclm)$s_id),
    ub = ordinal::ranef(object_cclm) + 2 * sqrt(ordinal::condVar(object_cclm)$s_id),
    s_id = rownames(ordinal::ranef(object_cclm)$s_id)
  )
  names(opm_ranef) <- c("estimate", "lb", "ub", "s_id")
  ranef_df_opm <- opm_ranef[order(opm_ranef$estimate), ] %>%
    rename(
      lb_intercept = lb,
      ub_intercept = ub,
      Intercept = estimate
    ) %>%
    mutate(
      Class = 1:dim(opm_ranef)[1],
      Sig = ifelse((lb_intercept > mean(Intercept)) | (ub_intercept < mean(Intercept)),
        "Excl. intercept", "Incl. intercept"
      )
    )
  return(ranef_df_opm)
}

plot_ranefs <- function(df) {
  return(ggplot(df, aes(x = Class, y = Intercept, color = Sig)) +
    geom_point(size = 0.3) +
    geom_errorbar(aes(x = Class, ymin = lb_intercept, ymax = ub_intercept),
      size = 0.2,
      alpha = 0.4
    ) +
    theme_bw() +
    scale_color_brewer(palette = "Set1") +
    labs(x = "School", y = "Estimate"))
}

calculate_opm_acc <- function(object, test_df, multilevel = F) {
  # Store cutoff points
  cutoffs <- c(-Inf, object$coefficients[1:(length(unique(train_df$y_cat)) - 1)], Inf)

  # Store fixed effect coefficients
  fixed_cos <- object$coefficients[length(unique(train_df$y_cat)):length(object$coefficients)]

  # Select and order regressors
  test <- test_df %>%
    dplyr::select(names(fixed_cos))

  if (multilevel) {
    # Store random effects in dataframe and include c_ids
    ranefs <- as.data.frame(ranef(object))
    ranefs$s_id <- rownames(ranefs)

    # Order random effects in accordance to the test set
    ranefs_ordered <- test_df %>%
      dplyr::select(s_id) %>%
      mutate(s_id = as.character(s_id)) %>%
      left_join(ranefs, by = "s_id") %>%
      dplyr::select(X.Intercept.)
    y_hat <- as.matrix(test) %*% fixed_cos + ranefs_ordered
  } else {
    y_hat <- as.matrix(test) %*% fixed_cos
  }
  y_cat_hat <- ifelse(y_hat < cutoffs[2], 1,
    ifelse(y_hat < cutoffs[3], 2,
      ifelse(y_hat < cutoffs[4], 3,
        ifelse(y_hat < cutoffs[5], 4, 5)
      )
    )
  )
  # return accuracy
  return(mean(y_cat_hat == test_df$y_cat))
}