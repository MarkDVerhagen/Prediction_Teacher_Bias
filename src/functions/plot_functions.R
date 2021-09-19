delist_cross <- function(df, col_names) {
  list_df <- c()
  first <- T
  i <- 1
  for (i in 1:length(df)) {
    new <- c(t(df[i]))[[1]]
    names(new) <- col_names
    if (first) {
      list_df <- new
      first <- F
    } else {
      list_df <- rbind(list_df, new)
    }
  }
  rownames(list_df) <- NULL
  list_df <- list_df / rowSums(list_df)
  return(list_df)
}

cross_plot <- function(df1, df2, first = T, color1 = "#EE0000FF",
                       color2 = "#3B4992FF") {
  df_own <- df1
  df_other <- df2
  if (mean(df_own %*% c(1, 2, 3, 4, 5)) > mean(df_other %*% c(1, 2, 3, 4, 5))) {
    offset <- 1
  } else {
    offset <- -1
  }

  plot_df <- data.frame(df_own, df_other) %>%
    reshape2::melt() %>%
    group_by(variable) %>%
    summarise(
      mean = mean(value),
      ptile_5 = quantile(value, probs = 0.05, na.rm = TRUE),
      ptile_95 = quantile(value, probs = 0.95, na.rm = TRUE)
    ) %>%
    mutate(
      model = ifelse(grepl("\\.1", variable), "Other model", "Own model"),
      x = gsub("\\.1", "", variable)
    )

  ggplot(plot_df, aes(x = x, y = mean, fill = model)) +
    geom_bar(stat = "summary", position = position_dodge(), color = "black") +
    geom_errorbar(aes(ymin = ptile_5, ymax = ptile_95),
      width = 0.2, position = position_dodge(width = 1)
    ) +
    geom_text(aes(label = paste0(round(mean, 3) * 100, "%")),
      position = position_dodge(width = 1), hjust = -.4, size = 4
    ) +
    coord_flip() +
    scale_fill_aaas(name = "Prediction model") +
    cowplot::theme_cowplot() +
    geom_vline(
      xintercept = mean(df_own %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
      color = color1
    ) +
    annotate("text",
      x = mean(df_own %*% c(1, 2, 3, 4, 5)) + 0.2 * offset,
      y = 0.6, color = color1, size = 4,
      label = as.character(round(mean(df_own %*% c(1, 2, 3, 4, 5)), 3))
    ) +
    geom_vline(
      xintercept = mean(df_other %*% c(1, 2, 3, 4, 5)), linetype = "dashed",
      color = color2
    ) +
    annotate("text",
      x = mean(df_other %*% c(1, 2, 3, 4, 5)) - 0.2 * offset,
      y = 0.6, color = color2, size = 4,
      label = as.character(round(mean(df_other %*% c(1, 2, 3, 4, 5)), 3))
    ) +
    theme(
      legend.position = "bottom",
      text = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    ) +
    ylim(c(0, 0.65)) +
    xlab("Predicted track level") +
    ylab("Prop. of prediction assigned to track")
}

delist_perf <- function(df,
                        col_names = c(
                          "lm_g", "lm_gender", "lm_ses", "lme_g",
                          "lme_gender", "lme_ses", "op_g", "op_gender",
                          "op_ses", "mop_g", "mop_gender", "mop_ses",
                          "full_g", "full_gender", "full_ses"
                        )) {
  list_df <- c()
  first <- T

  for (i in 1:dim(df)[1]) {
    new <- as.data.frame(t(unlist(df[i, ])))
    names(new) <- col_names
    if (first) {
      list_df <- new
      first <- F
    } else {
      list_df <- rbind(list_df, new)
    }
  }
  return(list_df)
}

predict_lm <- function(c) {
  return(ifelse(c < 1, 1,
    ifelse(c > 5, 5, c)
  ))
}

lm_ap <- function(mod, test) {
  coefs <- names(mod$coefficients)
  coefs <- coefs[!grepl("\\(", coefs)]

  return(data.frame(
    actuals = test$y_cat,
    predicts = as.matrix(data.frame(intercept = 1, test_df[coefs])) %*% mod$coefficients,
    correct = as.matrix(ifelse(predict_lm(round(predict(mod, test_df))) == test$y_cat,
      "Correct", "Wrong"
    ))
  ))
}

lme_ap <- function(mod, test) {
  summod <- summary(mod)
  coefs <- names(data.frame(summod$coefficients)$Estimate)
  coefs_names <- rownames(data.frame(summod$coefficients))
  coefs_final <- coefs_names[!grepl("\\(", coefs_names)]
  coefs <- data.frame(summod$coefficients)$Estimate[!grepl("\\(", coefs_names)]

  fixefs <- as.matrix(test[coefs_final]) %*% coefs
  return(data.frame(
    actuals = test$y_cat,
    predicts = fixefs,
    correct = ifelse(predict_lm(round(fixefs)) == test$y_cat,
      "Correct", "Wrong"
    )
  ))
}


plot_matrix <- function(ap_df, perf, cuts) {
  c1_sum <- ap_df %>%
    group_by(actuals) %>%
    summarise(perf = mean(correct == "Correct"))

  return(ggplot(data = ap_df) +
    geom_jitter(aes(x = predicts, y = actuals, color = correct), size = 0.3) +
    geom_vline(xintercept = cuts, linetype = "dashed") +
    cowplot::theme_cowplot() +
    ggtitle(paste0(
      "Predicted and observed track levels. Accuracy = ",
      round(perf, 2) * 100, "%"
    )) +
    xlab("Fixed effect including mapping to outcome variables") +
    ylab("Observed track outcome") +
    theme(
      panel.grid.major.y = element_line(size = 0.5, colour = "lightgrey"),
      panel.grid.minor.y = element_line(size = 0.25, colour = "lightgrey")
    ) +
    scale_color_aaas(name = "Prediction") +
    geom_text(
      data = c1_sum, aes(
        y = actuals, label = paste0(round(perf, 3) * 100, "%"),
        x = max(ap_df$predicts) + 0.75
      ),
      size = 5
    ) +
    xlim(0, max(ap_df$predicts) + 1.25) +
    theme(
      text = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    ))
}

ap_cuts <- function(mod, test) {
  linspace <- as.matrix(test[names(mod$coefficients)]) %*% mod$coefficients
  linspace <- linspace[order(linspace)]

  zetas <- mod$zeta
  df <- data.frame(
    p1 = pnorm(zetas[1] - linspace, 0, 1),
    p2 = pnorm(zetas[2] - linspace, 0, 1) - pnorm(zetas[1] - linspace, 0, 1),
    p3 = pnorm(zetas[3] - linspace, 0, 1) - pnorm(zetas[2] - linspace, 0, 1),
    p4 = pnorm(zetas[4] - linspace, 0, 1) - pnorm(zetas[3] - linspace, 0, 1),
    p5 = 1 - pnorm(zetas[4] - linspace, 0, 1)
  )

  final_df <- data.frame(
    fixed = linspace,
    prediction = apply(df, 1, function(x) which.max(x))
  )
  cuts <- final_df %>%
    arrange(fixed) %>%
    filter(
      !duplicated(prediction),
      prediction != 1
    )
  return(cuts$fixed)
}

generate_ranef_df <- function(object_lm, overall_intercept) {
  ranef_se <- arm::se.ranef(object_lm)
  ranef_df <- data.frame(
    school = 1:length(coef(object_lm)$s_id[, 1]),
    intercept = coef(object_lm)$s_id[, 1],
    se = ranef_se$s_id,
    s_id = rownames(ranef_se$s_id)
  )
  names(ranef_df) <- c("School", "Intercept", "SE", "s_id")

  ranef_df <- ranef_df %>%
    mutate(
      lb_intercept = -2 * SE + Intercept,
      ub_intercept = 2 * SE + Intercept
    )

  ranef_df <- ranef_df[order(ranef_df$Intercept), ] %>%
    mutate(
      Class = 1:dim(ranef_df)[1],
      Sig = ifelse((lb_intercept > overall_intercept) |
        (ub_intercept < overall_intercept),
      "Excl. intercept", "Incl. intercept"
      )
    )
  return(ranef_df)
}

normalize_op <- function(df_op, df_lm, maths = T) {
  if (maths) {
    ratio <- (df_lm$Estimate[rownames(df_lm) == "mean_Maths"]) /
      (df_op$Estimate[df_op$labels == "mean_Maths"])

    return(df_op %>%
      filter(!grepl("\\|", labels)) %>%
      mutate(
        Estimate = Estimate * ratio,
        `Std. Error` = `Std. Error` * ratio
      ))
  } else {
    diff <- (df$Estimate[df$labels == "4|5"] -
      df$Estimate[df$labels == "1|2"]) / 3
    return(df %>%
      filter(!grepl("\\|", labels)) %>%
      mutate(
        Estimate = Estimate / diff,
        `Std. Error` = `Std. Error` / diff
      ))
  }
}