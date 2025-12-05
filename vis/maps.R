library(ggplot2)
library(tidyverse)
library(ggtext)
library(sf)
library(RColorBrewer)
library(gt)
library(readr)
library(patchwork)
library(ggplotify)
library(gridExtra)
library(readxl)
library(haven)
library(ggpubr)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
#####################
## REGRESSION MAPS ##
#####################
reg_data <- st_read(file.path(folder, '..', 'results', 'final', 'coefficients',
                              'counties_coefficients.shp'))

#######################
## COEFFICIENT PLOTS ##
#######################
coeff_data <- reg_data[, c('STATEABBRV', 'STATE', 'S_PM25_MEA', 'S_AGE_CAT', 
                           'S_WALKABIL', 'S_POVERTY_', 'geometry')]
coeff_data <- coeff_data %>%
  pivot_longer(cols = c("S_PM25_MEA", "S_AGE_CAT", "S_WALKABIL", "S_POVERTY_"),
    names_to = "ind_variable", values_to = "coeff_values")

coeff_data$ind_variable <- factor(coeff_data$ind_variable,
  levels = c('S_AGE_CAT', 'S_PM25_MEA', 'S_WALKABIL', 'S_POVERTY_'),
  labels = c('Age', 'PM2.5 concentration', 'Walkability index', 
             'Poverty index'))

coeff_bins <- c(-Inf, -0.83, -0.22, 0, 0.41, 0.78, Inf)

coeff_data$coefficeint_bin <- cut(coeff_data$coeff_values, breaks = coeff_bins, 
     labels = c("Below -0.83", "-0.83 to -0.22", "-0.21 to 0", "0.1 to 0.41", 
     "0.42 to 0.78", "Above 0.78"))

bbox <- st_bbox(c(xmin = -124, xmax = -68, ymin = 22.9, ymax = 49), 
                crs = st_crs(4326))
bbox_proj <- st_transform(st_as_sfc(bbox), 5070)
bbox_proj_coords <- st_bbox(bbox_proj)

coeff_data <- coeff_data %>% filter(!STATEABBRV %in% c('AK', 'HI', 'PR'))
coeff_data <- st_transform(coeff_data, crs = 5070)

mgwr_variable <- ggplot() + 
  geom_sf(data = coeff_data, aes(fill = coefficeint_bin), linewidth = 0.001) +
  scale_fill_viridis_d(direction = 1) +
  labs(title = "(A) Multiscale Geographically Weighted Regression (MGWR) Maps.",
       subtitle = "MGWR coefficient values of independent variables across continental US counties (excluding counties from Alaska and Hawaii).", 
       fill = "Coefficients") + theme_minimal() +
  theme(legend.position = 'bottom',
    plot.margin = margin(0, 0, 0, 0),              
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
  guides(fill = guide_legend(ncol = 6)) +
  coord_sf(
    xlim = c(bbox_proj_coords["xmin"], bbox_proj_coords["xmax"]),
    ylim = c(bbox_proj_coords["ymin"], bbox_proj_coords["ymax"])) +
  facet_wrap( ~ ind_variable, ncol = 2, nrow = 2) 
  
#######################
## SIGNIFICANT PLOTS ##
#######################
sign_data <- reg_data[, c('STATEABBRV', 'STATE', 'S_SG_PM25_', 'S_SG_AGE_C', 
                           'S_SG_WALKA', 'S_SG_POVER', 'geometry')]

########################
## 1. Significant Age ##
########################
age_df <- sign_data %>%
  sf::st_drop_geometry() %>%
  dplyr::select(STATE, S_SG_AGE_C)


age_df <- age_df %>%
  filter(S_SG_AGE_C == 1)

age_df <- age_df %>%
  group_by(STATE) %>%
  summarise(totals = sum(S_SG_AGE_C, na.rm = TRUE))

age_df <- age_df %>%
  arrange(desc(totals)) %>%
  slice_head(n = 8)

county_counts <- data.frame(
  STATE = c("Texas","Missouri","Kansas","Kentucky","Illinois","Virginia",
            "Tennessee","Mississippi"),
  counties = c(254, 115, 105, 120, 102, 133, 95, 82))

age_df <- age_df %>%
  left_join(county_counts, by = "STATE")

age_df <- age_df %>%
  mutate(perc = (totals / counties)*100)

label_totals <- age_df %>%
  group_by(STATE) %>%
  summarize(total_value = sum(perc))

age_df$STATE <- factor(age_df$STATE,
    levels = c('Virginia', 'Texas', 'Kentucky', 'Illinois', 'Tennessee', 
               'Missouri', 'Kansas', 'Mississippi'),
    labels = c('Virginia', 'Texas', 'Kentucky', 'Illinois', 'Tennessee', 
               'Missouri', 'Kansas', 'Mississippi'))

age_plot <- ggplot(age_df, aes(x = STATE, y = perc, fill = STATE)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Percentage of Counties (%)",
       title = "(B) Top 8 States where Independent Varibales are Statistically Significant",
       subtitle = "Age") +
  geom_text(data = label_totals, aes(x = STATE, y = total_value, 
      label = sprintf("%.1f%%", total_value)), size = 2,
      position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  theme(
    legend.position = 'bottom',
    plot.margin = margin(t = 0, b = 0, l = 5, r = 5),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 6),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6.5),
    axis.text.y = element_text(size = 6.5),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 124))

##########################
## 2. Significant PM2.5 ##
##########################
pm_df <- sign_data %>%
  sf::st_drop_geometry() %>%
  dplyr::select(STATE, S_SG_PM25_)


pm_df <- pm_df %>%
  filter(S_SG_PM25_ == 1)

pm_df <- pm_df %>%
  group_by(STATE) %>%
  summarise(totals = sum(S_SG_PM25_, na.rm = TRUE))

pm_county_counts <- data.frame(
  STATE = c("Georgia", "Kansas", "Maryland", "Nebraska", "North Carolina",
            "Texas", "Virginia", "West Virginia" ),
  counties = c(159, 105, 24, 93, 100, 254, 133, 55))

pm_df <- pm_df %>%
  left_join(pm_county_counts, by = "STATE")

pm_df <- pm_df %>%
  mutate(perc = (totals / counties)*100)

pm_df$STATE <- factor(pm_df$STATE,
    levels = c('Georgia', 'West Virginia', 'Texas', 'Kansas', 'Nebraska',
               'North Carolina', 'Virginia', 'Maryland'),
    labels = c('Georgia', 'W. Virginia', 'Texas', 'Kansas', 'Nebraska',
               'N. Carolina', 'Virginia', 'Maryland'))

label_totals <- pm_df %>%
  group_by(STATE) %>%
  summarize(total_value = mean(perc))


pm_plot <- ggplot(pm_df, aes(x = STATE, y = perc, fill = STATE)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  geom_text(data = label_totals, aes(x = STATE, y = total_value, 
    label = sprintf("%.1f%%", total_value)), size = 2,
    position = position_dodge(0.9), vjust = 0.5, hjust = -0.1) +
  labs(x = NULL,
       y = "Percentage of Counties (%)",
       title = " ",
       subtitle = "PM2.5 concentration") +
  theme(
    legend.position = 'bottom',
    plot.margin = margin(t = 0, b = 0, l = 5, r = 5),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 6),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6.5),
    axis.text.y = element_text(size = 6.5),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
 labels = function(y)format(y, scientific = FALSE), limit = c(0, 39))

################################
## 3. Significant Walkability ##
################################
wk_df <- sign_data %>%
  sf::st_drop_geometry() %>%
  dplyr::select(STATE, S_SG_WALKA)


wk_df <- wk_df %>%
  filter(S_SG_WALKA == 1)

wk_df <- wk_df %>%
  group_by(STATE) %>%
  summarise(totals = sum(S_SG_WALKA, na.rm = TRUE))

wk_county_counts <- data.frame(
  STATE = c("Colorado","Georgia", "Kansas", "Maryland", "Nebraska",
            "Texas", "Virginia", "West Virginia"),
  counties = c(64, 159, 105, 24, 93, 254, 133, 55))

wk_df <- wk_df %>%
  left_join(wk_county_counts, by = "STATE")

wk_df <- wk_df %>%
  mutate(perc = (totals / counties)*100)

wk_df$STATE <- factor(wk_df$STATE,
    levels = c('Georgia', 'West Virginia', 'Texas', 'Colorado', 'Virginia',  
               'Nebraska', 'Maryland', 'Kansas'),
    labels = c('Georgia', 'W. Virginia', 'Texas', 'Colorado', 'Virginia',  
               'Nebraska', 'Maryland', 'Kansas'))

label_totals <- wk_df %>%
  group_by(STATE) %>%
  summarize(total_value = sum(perc))

wk_plot <- ggplot(wk_df, aes(x = STATE, y = perc, fill = STATE)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Percentage of Counties (%)",
       title = " ",
       subtitle = "Walkability index") +
  geom_text(data = label_totals, aes(x = STATE, y = total_value, 
      label = sprintf("%.1f%%", total_value)), size = 2,
      position = position_dodge(0.9), vjust = 0.5, hjust = -0.1)  +
  theme(
    legend.position = 'bottom',
    plot.margin = margin(t = 0, b = 0, l = 5, r = 5),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 6),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6.5),
    axis.text.y = element_text(size = 6.5),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
     labels = function(y)format(y, scientific = FALSE), limit = c(0, 51))

############################
## 4. Significant Poverty ##
############################
pv_df <- sign_data %>%
  sf::st_drop_geometry() %>%
  dplyr::select(STATE, S_SG_POVER)

pv_df <- pv_df %>%
  filter(S_SG_POVER == 1)

pv_df <- pv_df %>%
  group_by(STATE) %>%
  summarise(totals = sum(S_SG_POVER, na.rm = TRUE))

pv_county_counts <- data.frame(
  STATE = c("Colorado", "Georgia", "Maryland", "Nebraska",  "North Carolina",
            "South Dakota", "Texas", "Virginia"),
  counties = c(64, 159, 24, 93, 100, 66, 254, 133))

pv_df <- pv_df %>%
  left_join(pv_county_counts, by = "STATE")

pv_df <- pv_df %>%
  mutate(perc = (totals / counties)*100)

pv_df$STATE <- factor(pv_df$STATE,
    levels = c('Georgia', 'South Dakota', 'Virginia', 'Colorado', 
               'North Carolina', 'Texas', 'Nebraska', 'Maryland'),
    labels = c('Georgia', 'S. Dakota', 'Virginia', 'Colorado', 
               'N. Carolina', 'Texas', 'Nebraska', 'Maryland'))

label_totals <- pv_df %>%
  group_by(STATE) %>%
  summarize(total_value = sum(perc))

pv_plot <- ggplot(pv_df, aes(x = STATE, y = perc, fill = STATE)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Percentage of Counties (%)",
       title = " ",
       subtitle = "Poverty index") +
  geom_text(data = label_totals, aes(x = STATE, y = total_value, 
      label = sprintf("%.1f%%", total_value)), size = 2,
      position = position_dodge(0.9), vjust = 0.5, hjust = -0.1)  +
  theme(
    legend.position = 'bottom',
    plot.margin = margin(t = 0, b = 0, l = 5, r = 5),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 7),
    axis.title.x = element_text(size = 6),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6.5),
    axis.text.y = element_text(size = 6.5),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)) +
  scale_x_discrete(expand = c(0, 0.15)) + scale_y_continuous(expand = c(0, 0),
     labels = function(y)format(y, scientific = FALSE), limit = c(0, 34))

significant_plots <- ggarrange(age_plot, pm_plot, wk_plot, 
                               pv_plot, ncol = 4,
                             nrow = 1, heights = c(1, 1))

combined_plots <- ggarrange(mgwr_variable, significant_plots, ncol = 1,
                               nrow = 2, heights = c(4, 1))


path = file.path(folder, 'figures', 'significant_counties.png')
png(path, units="in", width=7, height=6.5, res=300)
print(combined_plots)
dev.off()

  


