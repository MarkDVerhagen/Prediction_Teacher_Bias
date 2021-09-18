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

## -- generate baseline theme
theme_custom <- theme(
  panel.grid.major.x =
    element_line(
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
)

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
  scale_color_aaas(
    name = "Parent's\neducation",
    labels = c(
      "PhD", "Master's", "Bachelor's",
      "Vocational: Level 3", "Vocational: Level 2",
      "Vocational: Level 1", "Primary Education"
    )
  ) +
  labs(x = "Year", y = "Difference (1 = full track)") +
  cowplot::theme_cowplot() +
  theme_custom +
  geom_hline(yintercept = 0, linetype = "dashed")

# save
ggsave(
  filename = "tex/plots/fig_desc_0.pdf", last_plot(),
  width = 10, height = 6
)

## FIGURE D.1: DISTRIBUTION TRACK LEVELS, TEST SCORES & RELATION
data <- readRDS("data/edit/analysis.rds")

scores_y <- data %>%
  select(mean_Reading, mean_Maths, mean_Language, y)

scores_y_melt <- reshape2::melt(id.vars = "y", scores_y)

scores_y_melt$variable <- gsub("mean_", "", scores_y_melt$variable)

set.seed(1)
samp <- scores_y_melt %>% sample_frac(0.025)

loess_fit <- ggplot(samp, aes(
  y = value * 100, x = as.factor(y),
  color = variable
)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  geom_jitter(alpha = 0.1) +
  facet_grid(rows = vars(variable)) +
  ggsci::scale_color_aaas(name = "Area") +
  theme_cowplot() +
  theme_custom +
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

(scores + ggtitle("")) + (loess_fit + ggtitle("") +
  theme(legend.position = "none")) +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "top"))

ggsave("tex/plots/fig_desc_1.pdf", last_plot(), width = 14, height = 10)


# Figure D.2: SAMPLE REPRENSTATIVITY --------------------------------------

# SAMPLE REPRESENTATION --------------------------------------------------------
# below script is generated internally by Data Provider to evaluate in-sample
# school level information with population CBS data from 2018-2019

## FIGURE D.1: SAMPLE REPRESENTATIVENESS
data <- readRDS("data/edit/analysis.rds")
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

schools <- readRDS("data/edit/schools.rds") %>%
  mutate(
    DEN = gsub("CON|PC|RE|RK", "Christian", DEN),
    DEN = gsub("OB", "Public", DEN),
    DEN = gsub("AB|OT", "Other", DEN)
  )

school_df <- schools %>%
  filter(!duplicated(s_id)) %>%
  left_join(school_size) %>%
  filter(!is.na(n)) %>%
  mutate(size = ifelse(n < 101, "<=100",
    ifelse(n < 201, "101-200",
      ifelse(n < 501, "201-500", "500+")
    )
  )) %>%
  select(-n)

size <- school_df %>%
  group_by(size) %>%
  tally() %>%
  mutate(prop = n / dim(school_df)[1]) %>%
  select(-n)

size$NL <- c(0.17, 0.33, 0.45, 0.05) # based on CBS
names(size) <- c("Var", "Data", "NL")

## SCHOOL WEIGHT
sw <- school_df %>%
  count(school_weight) %>%
  mutate(pop = n / dim(school_df)[1]) %>%
  select(-n)

cbs_sw <- read.csv2("data/raw/CBS_school_weights.csv")

sw_df <- rbind(
  data.frame(
    sample = "Population",
    sw = cbs_sw$ï..pop_school_weight
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
  select(-n)

denom$NL <- c(0.318 + 0.296, 0.081, 0.305)
names(denom) <- c("Var", "Data", "NL")

ste <- school_df %>%
  mutate(ste_mvs = abs(ste_mvs - 6)) %>%
  count(ste_mvs) %>%
  mutate(pop = n / 951) %>%
  select(-n)

ste$NL <- c(0.077, 0.214, 0.168, 0.309, 0.233)
names(ste) <- c("Var", "Data", "NL")

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
  filter(cohort == 2019) %>%
  group_by(y) %>%
  tally() %>%
  mutate(sample = n / sum(n)) %>%
  select(-n)

outcome_dist$population <- c(0.239, 0.271, 0.169, 0.102, 0.202)

outcome_dist_melt <- outcome_dist %>%
  reshape2::melt(id.vars = "y")
own_theme <- cowplot::theme_cowplot(font_size = 18) +

  ggplot2::theme(legend.position = "top")

font_size <- 5
text_size <- 28
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
  own_theme +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  theme(text = element_text(size = text_size))

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
  own_theme +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  theme(text = element_text(size = text_size))

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
  own_theme +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  theme(text = element_text(size = text_size))

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
  own_theme +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 0.7)) +
  theme(text = element_text(size = text_size))

plot_sw <- ggplot(data = sw_df, aes(x = sw, color = sample, fill = sample)) +
  geom_density(size = 1) +
  scale_color_manual(values = pal_aaas()(2)[2:1], name = "") +
  scale_fill_manual(values = pal_aaas(alpha = 0.3)(2)[2:1], name = "") +
  labs(
    x = "School disadvantage (higher = more disadvantaged)",
    y = "Density",
    title = "School disadvantage"
  ) +
  own_theme +
  background_grid(major = "y", minor = "y") +
  theme(axis.title.x = element_text(size = 13)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.13)) +
  theme(text = element_text(size = text_size))

plot_y <- ggplot(
  data = outcome_dist_melt,
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
    y = "Sample proportion", x = "Assigned Track Level",
    title = "Assigned Track Level"
  ) +
  own_theme +
  background_grid(major = "y", minor = "y") +
  scale_x_continuous(
    labels = c("I", "II", "III", "IV", "V"),
    breaks = c(1, 2, 3, 4, 5)
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6)) +
  theme(text = element_text(size = text_size))

theme_grid_rep <- theme(
  legend.position = "none",
  axis.text = element_text(size = 13),
  title = element_text(size = 11)
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
ggsave("tex/plots/fig_desc_2.pdf",
  width = 12, height = 8,
  school_sample
)