library(ggplot2)
library(ggpubr)
library(dplyr)
library(RColorBrewer)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

############
## 1. SEX ##
############
sex <- read.csv(file.path(folder, '..', 'results', 'processed', 
                          'sex_cdc_pulmonary_data.csv'))

df1 = sex %>%
  group_by(sex, fileyear) %>%
  summarise(sum = sum(mort_count))

df1$sex <- factor(df1$sex,
    levels = c('M', 'F'),
    labels = c('Male', 'Female'))

sex_plot <- ggplot(df1, aes(fileyear, sum, color = sex)) + 
    geom_line(position = position_dodge(width = 0.5), size = 0.5) +
  scale_color_viridis_d(direction = -1) +
  labs(colour = NULL, title = "US Pulmonary Embolism Mortality Counts.",
       subtitle = "(A) Cases by Sex",
       x = "Years", y = bquote("Mortality counts")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.ticks.y = element_line(linewidth = 0.2),
    axis.ticks.x = element_line(linewidth = 0.2)) + 
  guides(color = guide_legend(ncol = 6, title = "Sex")) +
  scale_y_continuous(expand = c(0, 0),
    labels = function(y) format(y, scientific = FALSE), limits = c(0, 5500)) +
  scale_x_continuous(expand = c(0, 0),
    limits = c(min(df1$fileyear, na.rm = TRUE), 2023),
    breaks = seq(min(df1$fileyear, na.rm = TRUE), 2023, by = 5))

############
## 2. AGE ##
############
age <- read.csv(file.path(folder, '..', 'results', 'processed', 
                          'age_cdc_pulmonary_data.csv'))

df2 = age %>%
  group_by(age_cat, fileyear) %>%
  summarise(sum = sum(mort_count))

df2 <- df2 %>%
  filter(!is.na(age_cat) & age_cat != "")


df2$age_cat <- factor(df2$age_cat,
    levels = c(' 9 years or below', '10 - 29 years', '30 - 49 years', '70 years or above'),
    labels = c('Below 9 Years', '10 - 29 Years', '30 - 49 Years', 'Above 70 Years'))

age_plot <- ggplot(df2, aes(fileyear, sum, color = age_cat)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) +
  scale_color_viridis_d(direction = -1) +
  labs(colour = NULL, title = " ",
       subtitle = "(B) Cases by Age",
       x = "Years", y = bquote("Mortality counts")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.ticks.y = element_line(linewidth = 0.2),
    axis.ticks.x = element_line(linewidth = 0.2)) + 
  guides(color = guide_legend(ncol = 6, title = "Age")) +
  scale_y_continuous(expand = c(0, 0),
     labels = function(y) format(y, scientific = FALSE), limits = c(0, 1700)) +
  scale_x_continuous(expand = c(0, 0),
     limits = c(min(df1$fileyear, na.rm = TRUE), 2023),
     breaks = seq(min(df1$fileyear, na.rm = TRUE), 2023, by = 5))

#############
## 3. RACE ##
#############
race <- read.csv(file.path(folder, '..', 'results', 'processed', 
                           'race_cdc_pulmonary_data.csv'))

df3 = race %>%
  group_by(race_recode3, fileyear) %>%
  summarise(sum = sum(mort_count))

df3 <- df3 %>%
  filter(!is.na(race_recode3) & race_recode3 != "")

race_plot <- ggplot(df3, aes(fileyear, sum, color = race_recode3)) + 
  geom_line(position = position_dodge(width = 0.5), size = 0.5) +
  scale_color_viridis_d(direction = -1) +
  labs(colour = NULL, title = " ",
       subtitle = "(C) Cases by Race",
       x = "Years", y = bquote("Mortality counts")) + 
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.spacing = unit(0.6, "lines"),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.ticks.y = element_line(linewidth = 0.2),
    axis.ticks.x = element_line(linewidth = 0.2)) + 
  guides(color = guide_legend(ncol = 6, title = "Race")) +
  scale_y_continuous(expand = c(0, 0),
     labels = function(y) format(y, scientific = FALSE), limits = c(0, 8000)) +
  scale_x_continuous(expand = c(0, 0),
       limits = c(min(df3$fileyear, na.rm = TRUE), 2023),
       breaks = seq(min(df3$fileyear, na.rm = TRUE), 2023, by = 5))





