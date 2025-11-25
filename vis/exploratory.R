library(ggplot2)
library(ggpubr)
library(dplyr)
library(RColorBrewer)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

############
## 1. SEX ##
############
sex_file <- read.csv(file.path(folder, '..', 'results', 'processed', 
                          'sex_cdc_pulmonary_data.csv'))

df1 = sex_file %>%
  group_by(sex, fileyear) %>%
  summarise(sum = sum(mort_count))

df1$sex <- factor(df1$sex,
    levels = c('M', 'F'),
    labels = c('Male', 'Female'))

sex_plot <- ggplot(df1, aes(fileyear, sum, color = sex)) + 
    geom_line(position = position_dodge(width = 0.5), size = 0.5) +
  scale_color_viridis_d(direction = -1) +
  labs(colour = NULL, title = "(A) Sex",
       subtitle = "Pulmonary embolism mortality \ncounts by sex across all US counties.",
       x = "Years", y = bquote("Mortality counts")) + theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
  ) + guides(color = guide_legend(nrow = 2, title = "Sex")) + 
  scale_y_continuous(expand = c(0, 0),
  labels = function(y) format(y, scientific = FALSE), limits = c(0, 5500)) +
  scale_x_continuous(expand = c(0, 0),
     limits = c(min(df1$fileyear, na.rm = TRUE), 2023),
     breaks = seq(min(df1$fileyear, na.rm = TRUE), 2023, by = 5))

############
## 2. AGE ##
############
age_file <- read.csv(file.path(folder, '..', 'results', 'processed', 
                          'age_cdc_pulmonary_data.csv'))
df2 <- age_file %>%
  filter(age_cat != " 9 years or below") %>%      
  group_by(age_cat, fileyear) %>%
  summarise(sum = sum(mort_count), .groups = "drop")

df2 <- df2 %>%
  filter(!is.na(age_cat) & age_cat != "")


df2$age_cat <- factor(df2$age_cat,
    levels = c('10 - 29 years', '30 - 49 years', '70 years or above'),
    labels = c('10 - 29 Years', '30 - 49 Years', 'Above 70 Years'))

age_plot <- ggplot(df2, aes(fileyear, sum, color = age_cat)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) +
  scale_color_viridis_d(direction = -1) +
  labs(colour = NULL, title = "(B) Age groups",
       subtitle = "Pulmonary embolism mortality counts by age groups across all US counties.",
       x = "Years", y = bquote("Mortality counts")) + theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)) + 
  guides(color = guide_legend(ncol = 2, title = "Age")) +
  scale_y_continuous(expand = c(0, 0),
     labels = function(y) format(y, scientific = FALSE), limits = c(0, 8000)) +
  scale_x_continuous(expand = c(0, 0),
     limits = c(min(df1$fileyear, na.rm = TRUE), 2023),
     breaks = seq(min(df1$fileyear, na.rm = TRUE), 2023, by = 5))

#############
## 3. RACE ##
#############
race_file <- read.csv(file.path(folder, '..', 'results', 'processed', 
                           'race_cdc_pulmonary_data.csv'))
df3 <- race_file %>%
  filter(race_recode3 != "Non-White/Black") %>%  
  group_by(race_recode3, fileyear) %>%
  summarise(sum = sum(mort_count), .groups = "drop")

df3 <- df3 %>%
  filter(!is.na(race_recode3) & race_recode3 != "")

race_plot <- ggplot(df3, aes(fileyear, sum, color = race_recode3)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) +
  scale_color_viridis_d(direction = -1) +
  labs(colour = NULL, title = "(C) Race",
       subtitle = "Pulmonary embolism mortality \ncounts by racial groups across \nall US counties.",
       x = "Years", y = bquote("Mortality counts")) + theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)) + 
  guides(color = guide_legend(nrow = 2, title = "Race")) +
  scale_y_continuous(expand = c(0, 0),
     labels = function(y) format(y, scientific = FALSE), limits = c(0, 8000)) +
  scale_x_continuous(expand = c(0, 0),
       limits = c(min(df3$fileyear, na.rm = TRUE), 2023),
       breaks = seq(min(df3$fileyear, na.rm = TRUE), 2023, by = 5))

###################
## 4. CHI SQUARE ##
###################
chi <- read.csv(file.path(folder, '..', 'results', 'stat_test', 
                           'all_chi_test.csv'))

chi <- chi[, c("det_variable", "chi2", "p_value")]
chi$det_variable <- factor(chi$det_variable,
       levels = c('gender','age','race'),
       labels = c('Sex', 'Age group','Race'))

df_long <- chi %>%pivot_longer(cols = c(chi2, p_value),
    names_to = "description", values_to = "value")

df <- df_long %>% mutate(value = as.numeric(value))

# compute a tiny replacement for zero p-values (smallest non-zero p / 10)
min_nonzero_p <- df %>%filter(description == "p_value", value > 0) %>%
  summarise(minp = min(value, na.rm = TRUE)) %>%pull(minp)

# if there were no non-zero p-values, fall back to a safe tiny value:
if (is.infinite(min_nonzero_p) || is.na(min_nonzero_p)) {
  min_nonzero_p <- 1e-300}

tiny <- min_nonzero_p / 10

# create a plotting value: leave chi2 as-is, convert p_value -> -log10(p_adjusted)
df_plot <- df %>%mutate(value_plot = case_when(
      description == "p_value" ~ -log10(pmax(value, tiny)), TRUE ~ value))

# optional: create clearer labels for facet strips
df_plot <- df_plot %>%
  mutate(description = ifelse(description == "p_value", "p-value (-log10 transformed)", "chi2"))

df_plot$description <- factor(df_plot$description,
       levels = c('chi2','p-value (-log10 transformed)'),
       labels = c('Chi-square', 'p-value (log transformed)'))

facet_limits <- df_plot %>%
  group_by(description) %>%
  summarise(max_y = max(value_plot, na.rm = TRUE) * 1.30)



facet_limits <- df_plot %>%
  group_by(description) %>%
  summarise(max_y = max(value_plot, na.rm = TRUE) * 1.30)   # expand by 30%

df_plot2 <- df_plot %>%
  left_join(facet_limits, by = "description")


chi_plot <- ggplot(df_plot2, aes(x = det_variable, y = value_plot, fill = det_variable)) +
    geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(value_plot, 0), y = value_plot + 0.03 * max_y),   
    size = 2, hjust = 0) + geom_blank(aes(y = max_y)) + coord_flip() +
  facet_wrap(~ description, scales = "free") +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Test Statistics value: Chi-Square or -log10(p)",
       title = "(D) Chi-square Test.",
       subtitle = "A Chi-square test for association of categorical variables (race, age \nand gender) across US counties.") +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 7),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)) +
  scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
#################
## Panel Plots ##
#################
desc_plots <- 
  (sex_plot + age_plot + plot_layout(widths = c(0.3, 0.7))) /
  (race_plot + chi_plot + plot_layout(widths = c(0.30, 0.7))) +
  plot_layout(heights = c(1, 1))

path = file.path(folder, 'figures', 'descriptive_plots.png')
png(path, units="in", width=6.6, height=7, res=300)
print(desc_plots)
dev.off()

