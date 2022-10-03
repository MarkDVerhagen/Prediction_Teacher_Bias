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
source("repo/src/styles.R")
load("analysis/coefs_perfs.rda")
load("data/results/model_results.rda")
data <- readRDS("data/edit/analysis.rds")

## FIGURE 1: CHANCE INEQUALITY ACROSS TIME
## Source: Onderwijsinspectie
data <- readxl::read_xlsx("data/raw/toets_ongelijkheid_20092017.xlsx")
melt_data <- reshape2::melt(data) %>%
  mutate(
    value = value / 100,
    Educ = as.factor(Educ)
  )

# plot
ggplot(melt_data, aes(
  y = value, x = variable,
  color = factor(Educ, levels = c(
    "PhD", "Higher Education: Master",
    "Higher Education: Bachelor", "Vocational: Level 3", "Vocational: Level 2",
    "Vocational: Level 1", "Primary Education"
  )), group = Educ
)) +
  geom_point() +
  geom_line(size = 1) +
  scale_color_manual(
      name = "Parent's\neducation",
    labels = c(
      "PhD", "Master's", "Bachelor's",
      "Vocational: Level 3", "Vocational: Level 2",
      "Vocational: Level 1", "Primary Education"
    ),
  values = MetBrewer::met.brewer("Hokusai1")) +
  labs(x = "Year", y = "Difference (1 = full track)") +
  cowplot::theme_cowplot() +
  custom_theme(text_size = 16, ver = T) +
  geom_hline(yintercept = 0, linetype = "dashed")

# save
ggsave(
  filename = "tex/plots/fig_desc_0.pdf", last_plot(),
  width = 10, height = 6
)

## FIGURE D.1: DISTRIBUTION TRACK LEVELS, TEST SCORES & RELATION
data <- readRDS("data/edit/analysis.rds")

scores_y <- data %>%
  dplyr::select(mean_Reading, mean_Maths, mean_Language, y)

scores_y_melt <- reshape2::melt(id.vars = "y", scores_y)

scores_y_melt$variable <- gsub("mean_", "", scores_y_melt$variable)

set.seed(1)
samp <- scores_y_melt %>% sample_frac(0.025)

loess_fit <- ggplot(samp, aes(
  y = value * 100, x = as.factor(y)
)) +
geom_jitter(aes(color = variable), shape = 16, position = position_jitter(0.2), alpha = 0.7, size =2) +
  geom_violin(aes(fill = variable), outlier.shape = NA, width = 0.6, alpha = 0.7, color = "black") +
    geom_boxplot(outlier.shape = NA, width = 0.1) +
    facet_grid(rows = vars(variable)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Juarez"), name = "Test type") +
  scale_color_manual(values = MetBrewer::met.brewer("Juarez"), name = "Test type") +
  theme_cowplot() +
  custom_theme(text_size = 16, hor = T) +
  geom_smooth(se = F, width = 0.5) +
  guides(color = F) +
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  scale_x_discrete(
    labels = c("I", "II", "III", "IV", "V"),
    breaks = c(1, 2, 3, 4, 5)
  ) +
  labs(x = "Assigned track level", y = "Percentile score") +
  ylab("") +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.y = element_blank(), text = element_text(size = 24)
  )

scores <- ggplot(scores_y_melt, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.7) +
  facet_grid(variable ~ .) +
  coord_flip() +
  cowplot::theme_cowplot() +
  theme(strip.background = element_blank(), strip.text.y = element_blank()) +
  custom_theme(text_size = 24, hor = T) +
  scale_x_continuous(labels = c("0", "25", "50", "75", "100"), name = "Average ability score") +
  scale_fill_manual(values = MetBrewer::met.brewer("Juarez"), name = "Test type") +
  ylab("Density")
  

(scores + ggtitle("")) + (loess_fit + ggtitle("") +
  theme(legend.position = "none")) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "top"))

ggsave("tex/plots/fig_desc_1.pdf", last_plot(), width = 14, height = 10)


# Figure D.2: SAMPLE REPRESENTATIVITY --------------------------------------

# SAMPLE REPRESENTATION --------------------------------------------------------
# below script is generated internally by Data Provider to evaluate in-sample
# school level information with population CBS data from 2018-2019

## FIGURE D.1: SAMPLE REPRESENTATIVENESS
data <- readRDS("data/edit/final_analysis.rds")
school_size <- readRDS("data/raw/pupil4.rds") %>%
  rename(
    s_id = school_id,
    c_id = class_id,
    p_id = pupil_id,
    pupil_date_birth = pupil_month_birth
  ) %>%
  filter(school_year == "2019") %>%
  filter(!duplicated(p_id)) %>%
  filter(!is.na(s_id)) %>%
  count(s_id) %>%
  ungroup() %>%
  mutate(s_id = as.character(s_id))

n <- dim(school_size)[1]

schools_raw <- readRDS("data/edit/schools.rds") %>%
  mutate(
    DEN = gsub("CON|PC|RE|RK", "Christian", DEN),
    DEN = gsub("OB", "Public", DEN),
    DEN = gsub("AB|OT", "Other", DEN)
  )

schools <- df %>%
  filter(!duplicated(s_id))
schools <- schools %>%
  left_join(schools_raw[, c("s_id", "school_weight")])

school_df <- schools %>%
  filter(!duplicated(s_id)) %>%
  left_join(school_size) %>%
  filter(!is.na(n)) %>%
  mutate(size = ifelse(n < 101, "<=100",
    ifelse(n < 201, "101-200",
      ifelse(n < 501, "201-500", "500+")
    )
  )) %>%
  dplyr::select(-n)

size <- school_df %>%
  group_by(size) %>%
  tally() %>%
  mutate(prop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

size$NL <- c(0.17, 0.33, 0.45, 0.05) # based on CBS
names(size) <- c("Var", "Data", "Pop")

## SCHOOL WEIGHT
sw <- school_df %>%
  count(school_weight) %>%
  mutate(pop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

cbs_sw <- read.csv2("data/raw/CBS_school_weights.csv")

sw_df <- rbind(
  data.frame(
    sample = "Population",
    sw = cbs_sw$pop_school_weight
  ),
  data.frame(
    sample = "Sample",
    sw = school_df[, "school_weight"]
  )
)

denom <- school_df %>%
  mutate(DEN = paste0("S_DENOM_", as.character(DEN))) %>%
  mutate(DEN = ifelse(grepl("AB|CON|OT|RE", DEN),
    "DEN_OTHER", DEN
  )) %>%
  count(DEN) %>%
  mutate(prop = n / dim(school_df)[1]) %>%
  dplyr::select(-n)

denom$NL <- c(0.318 + 0.296, 0.081, 0.305)
names(denom) <- c("Var", "Data", "Pop")

ste <- school_df %>%
  mutate(ste_mvs = abs(as.numeric(ste_mvs) - 6)) %>%
  count(ste_mvs) %>%
  mutate(pop = n / nrow(school_df)) %>%
  dplyr::select(-n)

ste$NL <- c(0.077, 0.214, 0.168, 0.309, 0.233)
names(ste) <- c("Var", "Data", "Pop")

comparison <- rbind(size, denom, ste)
comparison$Type <- c(
  rep("Size", 4), rep("Denomination", 3),
  rep("Urbanity", 5)
)

comparison <- rbind(
  comparison,
  c("Pupil 0.3", 0.039, 0.0438, "Weight"),
  c("Pupil 1.2", 0.037, 0.0440, "Weight")
)

comp_melt <- comparison %>%
  reshape2::melt(id.vars = c("Var", "Type")) %>%
  mutate(value = as.numeric(value))
order_c <- c(
  "<=100", "101-200", "201-500", "500+",
  "S_DENOM_OB", "S_DENOM_PC", "S_DENOM_RK",
  "DEN_OTHER", 1, 2, 3, 4, 5, "Pupil 0.3", "Pupil 1.2"
)

outcome_dist <- data %>%
  filter(cohort_2019 == 1) %>%
  group_by(y_cat) %>%
  tally() %>%
  mutate(sample = n / sum(n)) %>%
  dplyr::select(-n)

outcome_dist$population <- c(0.239, 0.271, 0.169, 0.102, 0.202)

outcome_dist_melt <- outcome_dist %>%
  reshape2::melt(id.vars = "y_cat")

font_size <- 5
# text_size <- 28
plot_size <- ggplot(comp_melt, aes(
  x = value,
  y = factor(Var, levels = order_c), fill = variable
)) +
  geom_bar(
    data = comp_melt %>% filter(Type == "Size"), stat = "identity",
    position = "dodge", color = "black"
  ) +
  geom_text(
    data = comp_melt %>% filter(Type == "Size"),
    aes(label = paste0(
      formatC(value * 100, format = "f", digits = 0),
      "%"
    )),
    position = position_dodge(width = 0.9),
    color = "black", vjust = -0.3, size = font_size
  ) +
  coord_flip() +
  scale_fill_aaas(name = "", labels = c("Sample", "Population")) +
  scale_y_discrete(labels = c(
    "Less than\n100 pupils", "101-200\npupils",
    "201-500\npupils", "500+\npupils"
  )) +
  labs(x = "Sample proportion", y = "", title = "School size") +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  cowplot::theme_cowplot() +
  custom_theme(hor = T, text_size = 16)

plot_denom <- ggplot(comp_melt, aes(
  x = value,
  y = factor(Var,
    levels = c("S_DENOM_Public", "S_DENOM_Christian", "S_DENOM_Other")
  ),
  fill = variable
)) +
  geom_bar(
    data = comp_melt %>% filter(Type == "Denomination"),
    stat = "identity", position = "dodge",
    color = "black"
  ) +
  geom_text(
    data = comp_melt %>% filter(Type == "Denomination"),
    aes(label = paste0(formatC(value * 100, format = "f", digits = 0), "%")),
    position = position_dodge(width = 0.9), color = "black", vjust = -0.3,
    size = font_size
  ) +
  coord_flip() +
  scale_fill_aaas(name = "School Denomination") +
  scale_y_discrete(labels = c("Public", "Christian", "Other")) +
  labs(y = "", x = "", title = "School denomination") +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  cowplot::theme_cowplot() +
  custom_theme(hor = T, text_size = 16)

plot_urb <- ggplot(comp_melt, aes(
  x = value,
  y = factor(Var,
    levels = order_c
  ), fill = variable
)) +
  geom_bar(
    data = comp_melt %>% filter(Type == "Urbanity"),
    stat = "identity",
    position = "dodge",
    color = "black"
  ) +
  geom_text(
    data = comp_melt %>% filter(Type == "Urbanity"),
    aes(label = paste0(formatC(value * 100, format = "f", digits = 0), "%")),
    position = position_dodge(width = 0.9), color = "black", vjust = -0.3,
    size = font_size
  ) +
  coord_flip() +
  scale_fill_aaas(name = "School Urbanity") +
  scale_y_discrete(
    labels = c("Very\nlow", "Low", "Medium", "High", "Very\nhigh")
  ) +
  labs(y = "", x = "Sample proportion", title = "School urbanity") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  cowplot::theme_cowplot() +
  custom_theme(hor = T, text_size = 16)

plot_weight <- ggplot(
  comp_melt,
  aes(x = value, y = factor(Var, levels = order_c), fill = variable)
) +
  geom_bar(
    data = comp_melt %>% filter(Type == "Weight"),
    stat = "identity",
    position = "dodge",
    color = "black"
  ) +
  geom_text(
    data = comp_melt %>% filter(Type == "Weight"),
    aes(label = paste0(formatC(value * 100, format = "f", digits = 0), "%")),
    position = position_dodge(width = 0.9),
    color = "black",
    vjust = -0.3,
    size = font_size
  ) +
  coord_flip() +
  scale_fill_aaas(name = "Pupil Weight") +
  scale_y_discrete(labels = c("Low", "Lowest")) +
  labs(y = "", x = "", title = "Parental education") +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  cowplot::theme_cowplot() +
  custom_theme(hor = T, text_size = 16)

plot_sw <- ggplot(data = sw_df, aes(x = sw, color = sample, fill = sample)) +
  geom_density(size = 1) +
  scale_color_manual(values = pal_aaas()(2)[2:1], name = "") +
  scale_fill_manual(values = pal_aaas(alpha = 0.3)(2)[2:1], name = "") +
  labs(
    x = "School disadvantage\n(higher = more disadvantaged)",
    y = "Density",
    title = "School disadvantage"
  ) +
  background_grid(major = "y", minor = "y") +
  theme(axis.title.x = element_text(size = 13)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.13)) +
  cowplot::theme_cowplot() +
  custom_theme(hor = T, text_size = 16)

plot_y <- ggplot(
  data = outcome_dist_melt %>% mutate(y = as.numeric(y_cat)),
  aes(y = value, x = y, fill = variable)
) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(
    aes(label = paste0(formatC(value * 100, format = "f", digits = 0), "%")),
    position = position_dodge(width = 0.9),
    color = "black",
    vjust = -0.3,
    size = font_size
  ) +
  scale_fill_aaas(name = "Track outcome") +
  labs(
    y = "Sample proportion", x = "",
    title = "Assigned Track Level"
  ) +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(
    labels = c("I", "II", "III", "IV", "V"),
    breaks = c(1, 2, 3, 4, 5)
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6)) +
  cowplot::theme_cowplot() +
  custom_theme(hor = T, text_size = 16)

theme_grid_rep <- theme(
  legend.position = "none",
  axis.text = element_text(size = 16),
  title = element_text(size = 20)
)
plots <- plot_grid(plot_size + theme_grid_rep,
  plot_denom + theme_grid_rep,
  plot_urb + theme_grid_rep,
  plot_weight + theme_grid_rep,
  plot_y + theme_grid_rep,
  plot_sw + theme_grid_rep,
  nrow = 3
)

legend <- get_legend(plot_size +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "top", legend.justification = "center"))

plot_grid(legend,
  rel_heights = c(.05, .95),
  plots, nrow = 2, align = "h", axis = "t"
)

school_sample <- last_plot()
ggsave("./tex/figs/fig_desc_2.pdf",
  width = 12, height = 12,
  school_sample
)
