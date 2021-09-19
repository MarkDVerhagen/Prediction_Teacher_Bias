## --- Setup environment
# load libraries
packages <- c(
  "tidyverse", "caret", "stargazer", "lme4", "leaps", "gtools",
  "reshape2", "patchwork", "hrbrthemes", "cowplot", "showtext",
  "BBmisc", "ggthemes", "arm", "ggsci", "patchwork", "gridExtra"
)
lapply(packages, require, character.only = TRUE)

showtext::showtext_auto()

## -- load functions and saved coefficients and data
source("analysis/functions/analysis_functions.R")
source("analysis/functions/plot_functions.R")
load("analysis/coefs_perfs.rda")
load("data/results/model_results.rda")
data <- readRDS("data/edit/analysis.rds")

## FIGURE R.1: PERFORMANCE AND COEFFICIENTS OF BASIC MODELS

results <- readRDS("1_250_lm_lme_op_mop_perf.rds")
results_full <- readRDS("data/simulations/1_250_mop_full_perf.rds")
results_df <- as.data.frame(cbind(results, results_full))

perf <- delist_perf(results_df)

diff_df <- perf %>%
  mutate(
    lm_gender = lm_gender - lm_g,
    lm_ses = lm_ses - lm_g,
    lm_school = lme_g - lm_g,
    lme_gender = lme_gender - lme_g,
    lme_ses = lme_ses - lme_g,
    op_gender = op_gender - op_g,
    op_ses = op_ses - op_g,
    op_school = mop_g - op_g,
    mop_gender = mop_gender - mop_g,
    mop_ses = mop_ses - mop_g,
    full_gender = full_gender - full_g,
    full_ses = full_ses - full_g
  )

melted_lm <- reshape2::melt(perf %>%
  select(lm_g, lm_gender, lm_ses, lme_g))

means <- melted_lm %>%
  group_by(variable) %>%
  summarise(mean = mean(value))

ggplot(melted_lm, aes(x = variable, y = value)) +
  stat_summary(fun = "mean") +
  coord_flip() +
  cowplot::theme_cowplot()

melted <- reshape2::melt(perf)

desc_df <- melted %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    ptile_5 = quantile(value, probs = 0.05, na.rm = TRUE),
    ptile_95 = quantile(value, probs = 0.95, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    model = ifelse(grepl("lm", variable), "Interval", "Categorical"),
    type = ifelse(grepl("lme_|mop_", variable), "Including random intercept",
      ifelse(grepl("full", variable), "Full random effects",
        "No random effects"
      )
    ),
    spec = ifelse(grepl("ses", variable), "Incl. Parental\nEducation",
      ifelse(grepl("gender", variable), "Incl. Sex",
        ifelse(grepl("school", variable), "Incl. School\nEffects",
          "Ability only"
        )
      )
    )
  )

desc_df_full <- rbind(
  desc_df,
  desc_df[desc_df$variable %in% c("mop_g", "lme_g"), ] %>%
    mutate(
      variable = gsub("lme_g", "lm_school", variable),
      variable = gsub("mop_g", "op_school", variable)
    ) %>%
    mutate(
      spec = "Incl. School\nEffects",
      type = "No random effects"
    )
)

perf_plot <- ggplot(
  desc_df_full %>% filter(!grepl("Full ", type)) %>%
    filter(type == "No random effects"),
  aes(x = reorder(spec, -mean), y = mean, fill = model, group = type)
) +
  geom_bar(stat = "identity", position = position_dodge2(), color = "black") +
  geom_text(aes(label = paste0(round(mean, 3) * 100, "%")),
    position = position_dodge2(width = 0.9),
    hjust = -0.1, size = 5
  ) +
  coord_flip(ylim = c(0.56, 0.7)) +
  cowplot::theme_cowplot() +
  scale_fill_aaas(name = "Model type") +
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
  theme_custom +
  xlab("") +
  ylab("Out-of-sample predictive accuracy")


mop_df <- as.data.frame(summary(mop_fit)$coefficients)
lme_df <- as.data.frame(summary(re_fit)$coefficients)

lme_df_norm <- lme_df %>%
  mutate(var = row.names(lme_df)) %>%
  mutate(
    Estimate = Estimate / lme_df$Estimate[grepl("Maths", row.names(lme_df))],
    `Std. Error` = `Std. Error` /
      lme_df$Estimate[grepl("Maths", row.names(lme_df))]
  ) %>%
  filter(!grepl("tercept|cohort", var))

mop_df_norm <- mop_df %>%
  mutate(var = row.names(mop_df)) %>%
  mutate(
    Estimate = Estimate / mop_df$Estimate[grepl("Maths", row.names(mop_df))],
    `Std. Error` = `Std. Error` /
      mop_df$Estimate[grepl("Maths", row.names(mop_df))]
  ) %>%
  filter(
    !grepl("\\|", var),
    !grepl("tercept|cohort", var)
  )

full_models <- rbind(
  lme_df_norm %>%
    select(var, Estimate, `Std. Error`) %>%
    mutate(
      spec = "Ability, pupil, school",
      model = "Interval"
    ),
  mop_df_norm %>%
    select(var, Estimate, `Std. Error`) %>%
    mutate(
      spec = "Ability, pupil, school",
      model = "Categorical"
    )
)

full_models$var <- gsub(
  "mean_Reading", "Ability score:\nReading",
  full_models$var
)
full_models$var <- gsub(
  "mean_Maths", "Ability score:\nMaths",
  full_models$var
)
full_models$var <- gsub(
  "mean_Language", "Ability score:\nLanguage",
  full_models$var
)
full_models$var <- gsub(
  "pupil_gender", "Sex:\nFemale",
  full_models$var
)
full_models$var <- gsub(
  "pupil_low_ses", "Parental\nEducation:\nLow",
  full_models$var
)

ratio_models <- full_models %>%
  reshape2::dcast(var ~ model, value.var = "Estimate") %>%
  mutate(
    ratio = as.character(round(Categorical / Interval, 2)),
    model = "Categorical"
  )

coef_plot <-
  ggplot(full_models, aes(x = var, y = Estimate, color = as.factor(model))) +
  geom_point(size = 2, aes(color = as.factor(model))) +
  coord_flip() +
  geom_errorbar(aes(
    ymin = Estimate - 1.96 * `Std. Error`,
    ymax = Estimate + 1.96 * `Std. Error`
  ), width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
  ylim(-0.075, 1.2) +
  geom_text(
    data = ratio_models, aes(x = var, y = 1.1, label = ratio), color = "black",
    size = 5
  ) +
  cowplot::theme_cowplot(font_size = 15) +
  scale_color_aaas(name = "Model type") +
  theme(legend.position = "top") +
  theme_custom +
  xlab("") +
  ylab("Coefficient estimate normalised by coefficient for Maths score")

(coef_plot + ggtitle("Coefficient estimates") + perf_plot +
  ggtitle("OOS-predictive accuracy"))

ggsave("tex/plots/fig_res_1.pdf", last_plot(), width = 13, height = 7)

## FIGURE 2: PREDICTIVE VERSUS OBSERVED FOR LINEAR AND ORDERED OUTCOME

lm_aps <- lapply(fit_lm_p, FUN = function(x) lm_ap(x, test_df))
lme_aps <- lapply(fit_lme_p, FUN = function(x) lme_ap(x, test_df))

lm_ap_plot <- plot_matrix(lm_aps[[1]], lm_perf[[1]],
  cuts = c(1.5, 2.5, 3.5, 4.5)
)
lme_ap_plot <- plot_matrix(lme_aps[[1]], lme_perf[[1]],
  cuts = c(1.5, 2.5, 3.5, 4.5)
)

op_aps <- lapply(fit_op_p, FUN = function(x) {
  data.frame(
    actuals = test_df$y_cat,
    predicts = as.matrix(test_df[names(x$coefficients)]) %*% x$coefficients,
    correct = ifelse(predict(x, test_df) == test_df$y_cat, "Correct", "Wrong")
  )
})

op_cuts <- lapply(fit_op_p, FUN = function(x) ap_cuts(x, test_df))
op_perf <- lapply(fit_op_p, FUN = function(x) {
  mean(test_df$y_cat == predict(x, test_df))
})

op_ap_plot <- plot_matrix(op_aps[[1]], op_perf[[1]], cuts = op_cuts[[1]])

mop_perf <- lapply(fit_mop_p, FUN = function(x) {
  mean(predict_opm(x, test_df)$prediction == test_df$y_cat)
})
mop_cuts <- lapply(fit_mop_p, FUN = function(x) predict_opm(x, test_df)$cuts)
mop_aps <- lapply(fit_mop_p, FUN = function(x) predict_opm(x, test_df)$ap)

mop_ap_plot <- plot_matrix(mop_aps[[1]], mop_perf[[1]], cuts = mop_cuts[[1]])

lm_plot <- lme_ap_plot + ggtitle("Interval, ability + school effects") +
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  scale_color_manual(name = "Prediction", values = c("#EE0000FF", "#FFCCCC")) +
  theme(legend.position = "top") + ggtitle("Interval") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c(1, 2, 3, 4, 5)
  ) +
  scale_y_discrete(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("I", "II", "III", "IV", "V")
  )

mop_plot <- mop_ap_plot + ggtitle("Interval, ability + school effects") +
  ylab("") +
  guides(colour = guide_legend(override.aes = list(size = 2.5))) +
  scale_color_manual(name = "Prediction", values = c("#3B4992FF", "#DBDEF0")) +
  theme(legend.position = "top") + ggtitle("Categorical") +
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8, 10),
    labels = c(0, 2, 4, 6, 8, 10)
  ) +
  scale_y_discrete(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("I", "II", "III", "IV", "V")
  )

(lm_plot + mop_plot)

ggsave("tex/plots/fig_res_2.pdf", last_plot(), width = 13, height = 7)

## FIGURE 3 GROUP EFFECTS

# Teacher Bias: Cross Gender ---------------------------------------------------
cross_results_df <- readRDS("data/simulations/1_250_cross_within.rds")
names(cross_results_df) <- c(
  "ses_00", "ses_10", "ses_01", "ses_11", "sex_00",
  "sex_10", "sex_01", "sex_11"
)

df_res <- lapply(cross_results_df, FUN = function(x) {
  delist_cross(x, col_names = c("I", "II", "III", "IV", "V"))
})

df1 <- df_res$ses_00
df2 <- df_res$ses_10

ses_0 <- cross_plot(df_res$ses_00, df_res$ses_10) +
  scale_fill_aaas(
    name = "Prediction model",
    labels = c("Fit to low-SES", "Fit to high-SES")
  )
ses_1 <- cross_plot(df_res$ses_01, df_res$ses_11) +
  scale_fill_aaas(
    name = "Prediction model",
    labels = c("Fit to low-SES", "Fit to high-SES")
  )

sex_0 <- cross_plot(df_res$sex_00, df_res$sex_10,
  color1 = pal_aaas()(8)[8],
  color2 = pal_aaas()(8)[7]
) +
  scale_fill_manual(
    values = pal_aaas()(8)[7:8],
    name = "Prediction model",
    labels = c("Fit to girls", "Fit to boys")
  )

sex_1 <- cross_plot(df_res$sex_01, df_res$sex_11,
  color1 = pal_aaas()(8)[8],
  color2 = pal_aaas()(8)[7]
) +
  scale_fill_manual(
    values = pal_aaas()(8)[7:8],
    name = "Prediction model",
    labels = c("Fit to girls", "Fit to boys")
  )

ses_plot <- ((sex_0 + ggtitle("Boys") + ylab("") +
  sex_1 + ggtitle("Girls") + ylab("") + xlab("")) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    tag_levels = "I",
    theme = theme(legend.position = "bottom")
  ))

sex_plot <- ((ses_0 + ggtitle("High Parent. Educ.") +
  ses_1 + ggtitle("Low Parent. Educ.") + xlab("")) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    tag_levels = "I",
    theme = theme(legend.position = "bottom")
  ))
(ses_plot / sex_plot)

ggsave("tex/plots/fig_res_3.pdf", last_plot(), width = 13, height = 11)

## FIGURE 4 SCHOOL VARIATION IN SES AND SEX & PREDICTIVE PERFORMANCE
load("data/results/mop_s_results.rda")

custom_theme <- theme(
  panel.grid.major.x = element_line(
    size = 0.5, linetype = "dotted",
    colour = "lightgrey"
  ),
  panel.grid.minor.x = element_line(
    size = 0.25, linetype = "dotted",
    colour = "lightgrey"
  ),
  strip.placement = "outside",
  strip.text.y = element_text(
    face = "bold", hjust = 0.5, vjust = 0.5
  ),
  strip.background = element_rect(
    fill = NA, color = "black", size = 1.5
  ),
  panel.spacing.x = unit(0.08, "lines"),
  panel.spacing.y = unit(0.1, "lines"),
  panel.border = element_rect(
    color = "lightgrey", fill = NA, size = 0.5
  ),
  text = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16)
)

ranef_sex <- as.data.frame(ordinal::ranef(mop_fit_s_sex))
names(ranef_sex) <- c("Intercept", "Sex")
ranef_sex_melt <- ranef_sex %>%
  reshape2::melt()

ranef_ses <- as.data.frame(ordinal::ranef(mop_fit_s_ses))
names(ranef_ses) <- c("Intercept", "Ses")
ranef_ses_melt <- ranef_ses %>%
  reshape2::melt()

sex_re_plot <- ggplot(ranef_sex_melt, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.6) +
  cowplot::theme_cowplot() +
  ggsci::scale_fill_aaas(name = "Random effect") +
  xlab("Random effect") +
  ylab("Density") +
  theme(legend.position = "top") +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  custom_theme +
  theme(axis.text.y = element_blank())

ses_re_plot <- ggplot(ranef_ses_melt, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.6) +
  cowplot::theme_cowplot() +
  ggsci::scale_fill_aaas(name = "Random effect") +
  xlab("Random effect") +
  ylab("Density") +
  theme(legend.position = "top") +
  geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
  custom_theme +
  theme(axis.text.y = element_blank())


effect_plot <- ((sex_re_plot +
  scale_fill_manual(
    name = "Random effect",
    values = ggsci::pal_aaas("default")(8)[7:8]
  ) +
  ggtitle("Pupil Sex")) / (ses_re_plot +
  ggtitle("Pupil Parental Education"))) #+

## Performance including various school level variables / variations
perf_full <- readRDS("data/simulations/1_250_mop_full_perf.rds")

perf_full_s <- delist_perf(
  readRDS("data/simulations/1_250_mop_full_perf_complete.rds"),
  col_names = c("perf_full_re")
)

perf_lm <- rbind(
  readRDS("data/simulations/1_250_lm_lme_op_mop_perf.rds")
)

perf_df <- delist_perf(perf_school, col_names = c("den", "ste", "sw", "all"))
perf_lm <- delist_perf(perf_lm, col_names = c(
  "lm_g", "lm_gender", "lm_ses", "lme_g", "lme_gender",
  "lme_ses", "op_g", "op_gender", "op_ses", "mop_g",
  "mop_gender", "mop_ses"
))
perf_school_full <- delist_perf(perf_school_full,
  col_names = c("op_full", "mop_full")
)
perf_full_df <- delist_perf(perf_full, col_names = c("g", "sex", "ses"))

results_df <- data.frame(
  op_grades = perf_lm$op_g, op_full = perf_school_full$op_full,
  perf_df, ri_g = perf_school_full$mop_full,
  reshape2::melt()

means <- melt_df %>%
  group_by(variable) %>%
  summarise(mean = mean(value))

desc_df <- melt_df %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    ptile_5 = quantile(value, probs = 0.05, na.rm = TRUE),
    ptile_95 = quantile(value, probs = 0.95, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    type = ifelse(grepl("ri|s_|perf_full", variable),
      "Pupil-  & school-level\nrandom effects model",
      ifelse(grepl("op_grades|op_full", variable),
        "Pupil-level\nstandard model", "Pupil- & School-level\nstandard model"
      )
    ),
    spec = ifelse(grepl("ste", variable), "Incl. Urbanity",
      ifelse(grepl("den", variable), "Incl. Denomination",
        ifelse(grepl("sw", variable), "Incl. School-level\nParental education",
          ifelse(grepl("all", variable), "Incl. All School Vars",
            ifelse(grepl("ri_g", variable), "Controls +\nintercepts",
              ifelse(grepl("s_grades", variable),
                "Controls + \nintercepts & grades",
                ifelse(grepl("s_sex", variable),
                  "Controls + \nintercepts & sex",
                  ifelse(grepl("s_ses", variable),
                    "Controls + \nintercepts & ses",
                    ifelse(grepl("s_ses", variable),
                      "Controls + \nintercepts & ses",
                      ifelse(grepl("s_ability", variable),
                        "Controls + \nintercepts & ability",
                        ifelse(grepl("op_full", variable),
                          "All pupil variables",
                          ifelse(grepl("perf_full", variable),
                            "Full random effects", "Ability only"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

perf_plot <- ggplot(
  desc_df,
  aes(x = reorder(spec, mean), y = mean, fill = factor(type, levels = c(
    "Pupil-  & school-level\nrandom effects model",
    "Pupil- & School-level\nstandard model",
    "Pupil-level\nstandard model"
  )))
) +
  geom_bar(stat = "identity", position = position_dodge2(), color = "black") +
  geom_text(aes(label = ifelse(nchar(as.character(round(mean, 3) * 100)) == 2,
    paste0(as.character(round(mean, 3) * 100), ".0%"),
    paste0(as.character(round(mean, 3) * 100), "%")
  )), position = position_dodge2(width = 0.9), hjust = -0.1, size = 5) +
  coord_flip(ylim = c(0.625, 0.7)) +
  facet_grid(type ~ ., scales = "free", space = "free", switch = "y") +
  cowplot::theme_cowplot() +
  scale_fill_manual(values = pal_aaas()(7)[5:7], name = "Model type") +
  geom_hline(yintercept = 0, linetype = 2, color = "darkgrey") +
  theme(
    panel.grid.major.x = element_line(
      size = 0.5, linetype = "dotted",
      colour = "lightgrey"
    ),
    panel.grid.minor.x = element_line(
      size = 0.25, linetype = "dotted",
      colour = "lightgrey"
    ),
    strip.placement = "outside",
    strip.text.y = element_text(face = "bold", hjust = 0.5, vjust = 0.5),
    strip.background = element_rect(fill = NA, color = "black", size = 1.5),
    panel.spacing.x = unit(0.08, "lines"),
    panel.spacing.y = unit(0.1, "lines"),
    panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
    legend.position = "top",
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  ) +
  xlab("") +
  ylab("Out-of-sample predictive accuracy") +
  theme(legend.position = "none")

(perf_plot + ggtitle("OOS-predictive performance")) + (effect_plot)


ggsave("tex/plots/fig_res_4.pdf", last_plot(), width = 13, height = 8.5)

## FIGURE 5: SCHOOL EFFECTS
results_school <- readRDS("data/simulations/1_250_school.rds")
full_df <- c()

for (s in 1:length(results_school$V1)) {
  sub <- as.data.frame(results_school$V1[s])
  names(sub) <- c("s_id", "mean_full", "mean_none", "school_effect")
  full_df <- rbind(full_df, sub)
}

plot_df <- full_df %>%
  group_by(s_id) %>%
  summarise(
    mean_full = mean(mean_full),
    mean_none = mean(mean_none),
    mean_school_effect = mean(school_effect),
    ptile_5 = quantile(school_effect, probs = 0.05),
    ptile_95 = quantile(school_effect, probs = 0.95)
  ) %>%
  mutate(includes_zero = ifelse(((mean_school_effect < 0) & (ptile_95 < 0)) |
    ((mean_school_effect > 0) & (ptile_5 > 0)),
  "Excluding zero", "Including zero"
  ))

plot_df_ordered <- plot_df[order(plot_df$mean_school_effect), ]
plot_df_ordered$x <- as.numeric(1:dim(plot_df_ordered)[1])

school_plot <- ggplot(
  plot_df_ordered,
  aes(x = x, y = mean_school_effect, color = includes_zero)
) +
  geom_point() +
  geom_errorbar(aes(x = x, ymin = ptile_5, ymax = ptile_95),
    size = 0.2,
    alpha = 0.4
  ) +
  theme(axis.title.x = element_blank()) +
  cowplot::theme_cowplot() +
  scale_color_aaas(name = "Interval") +
  xlab("Schools") +
  ylab(paste0(
    "Difference in school's mean predicted ",
    "track level relative to expected"
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plot_df_ordered$includes_zero_or_zero <- ifelse(
  plot_df_ordered$includes_zero == "Excluding zero",
  "Excluding zero",
  ifelse((plot_df_ordered$ptile_5 == 0) | (plot_df_ordered$ptile_95 == 0),
    "Ends at zero", "Including zero"
  )
)

school_plot <- ggplot(plot_df_ordered, aes(
  x = x, y = mean_school_effect,
  color = factor(includes_zero_or_zero,
    levels = c("Excluding zero", "Including zero", "Ends at zero")
  )
)) +
  geom_point() +
  geom_errorbar(aes(x = x, ymin = ptile_5, ymax = ptile_95),
    size = 0.2,
    alpha = 0.4
  ) +
  theme(axis.title.x = element_blank()) +
  cowplot::theme_cowplot() +
  scale_color_aaas(name = "Interval") +
  xlab("Schools") +
  ylab("Diff. predicted mean track level using school vs. population model") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )

school_plot + theme(legend.position = "top") + ggtitle("School variability")

ggsave("tex/plots/fig_res_5.pdf", last_plot(), width = 13, height = 7.5)