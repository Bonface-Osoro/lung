library(ggplot2)
library(ggpubr)
library(sf)
library(dplyr)
library(stringr)
library(tidyr)
library(jsonlite)
library(tidycensus)
census_api_key('c5309bbf9afe044d84e8979c33e4181ab9d6f885', install = TRUE)

suppressMessages(library(tidyverse))
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
#####################
## REGRESSION MAPS ##
#####################
national_shp <- st_read(file.path(folder, '..', 'data', 'raw', 'shapefiles',
                                   'US_outline.shp'))
national_shp <- st_transform(national_shp, crs = 5070)

counties_data <- st_read(file.path(folder, '..', 'data', 'raw', 'shapefiles',
                              'US_counties_air.shp'))

counties_data <- counties_data %>%
  mutate(FIPS = str_pad(as.character(FIPS), 5, pad = '0'))

data_2010 <- read.csv(file.path(folder, '..', 'results', 'final', 
                                 'gwr_poisson_2005_2010_results.csv'))

data_2017 <- read.csv(file.path(folder, '..', 'results', 'final', 
                                'gwr_poisson_2011_2017_results.csv'))

data_2022 <- read.csv(file.path(folder, '..', 'results', 'final', 
                                'gwr_poisson_2018_2022_results.csv'))

data_all <- bind_rows(data_2010, data_2017, data_2022)

data_all <- data_all %>%
  select(fips, county_name, res_statefips, latitude, longitude, pm_25, epoch,
         CSMOKING_CrudePrev, OBESITY_CrudePrev, unemployment_rate, 
         sig_pm_25, sig_CSMOKING_CrudePrev, sig_OBESITY_CrudePrev, 
         sig_unemployment_rate)

variable_labels <- c(
  pm_25                = 'PM2.5 Concentration',
  CSMOKING_CrudePrev   = 'Current Smoking (%)',
  OBESITY_CrudePrev    = 'Obesity (%)',
  unemployment_rate    = 'Unemployment Rate (%)')

sig_lookup <- c(
  pm_25                = 'sig_pm_25',
  CSMOKING_CrudePrev   = 'sig_CSMOKING_CrudePrev',
  OBESITY_CrudePrev    = 'sig_OBESITY_CrudePrev',
  unemployment_rate    = 'sig_unemployment_rate')

data_all <- data_all %>%pivot_longer(
    cols = c(pm_25, CSMOKING_CrudePrev, OBESITY_CrudePrev, unemployment_rate),
    names_to  = 'variable', values_to = 'value') %>% rowwise() %>%
  mutate(sig_col = sig_lookup[variable], significant = as.integer(data_all[
      data_all$fips == fips & data_all$epoch == epoch,sig_col][1])) %>%
  ungroup() %>%
  select(-sig_col) %>% mutate(variable_label = factor(variable_labels[variable],
      levels = unname(variable_labels)))

data_all <- data_all %>%
  mutate(fips = str_pad(as.character(as.integer(fips)), 5, pad = '0'))

counties_all <- counties_data %>%
  left_join(
    st_drop_geometry(data_all),
    by = c('FIPS' = 'fips')
  )

###################################
## 2005 - 2010 SIGNIFICANT PLOTS ##
###################################
counties_2005 <- counties_all %>%
  filter(epoch == '2005-2010')

histo_2005 <- ggplot(counties_2005, aes(x = value, fill = factor(significant))) +
  geom_histogram(bins = 40, colour = 'white', linewidth = 0.2) +
  facet_wrap(~ variable_label, scales = 'free', ncol = 5) +
  scale_fill_manual(
    values = c('0' = 'grey70', '1' = '#d73027'),
    labels = c('0' = 'Not significant', '1' = 'Significant'),
    name   = NULL)

plot_2005 <- ggplot() +
  geom_sf(data = national_shp, fill = 'bisque3', size = 0.05) +
  geom_sf(data = counties_2005, aes(fill = factor(significant)), color = NA) +
  scale_fill_manual(values = c('0' = 'bisque3', '1' = 'tomato3'),
    labels = c('0' = 'Not significant', '1' = 'Significant'),
    name   = 'Significance') +
  facet_wrap(~ variable_label, ncol = 4) +
  labs(title = "Poisson Geographically Weighted Regression.",
       subtitle = "(A) 2005 - 2010: Spatial extent of each variable's significant association with PE mortality in US counties.", 
       fill = "Significance") + theme_minimal() +
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
        legend.text = element_text(size = 7)) 

###################################
## 2011 - 2017 SIGNIFICANT PLOTS ##
###################################
counties_2011 <- counties_all %>%
  filter(epoch == '2011-2017')

plot_2011 <- ggplot() +
  geom_sf(data = national_shp, fill = 'bisque3', size = 0.001) +
  geom_sf(data = counties_2011, aes(fill = factor(significant)), color = NA) +
  scale_fill_manual(values = c('0' = 'bisque3', '1' = 'tomato3'),
                    labels = c('0' = 'Not significant', '1' = 'Significant'),
                    name   = 'Significance') +
  facet_wrap(~ variable_label, ncol = 4) +
  labs(title = " ",
       subtitle = "(B) 2011 - 2017: Spatial extent of each variable's significant association with PE mortality in US counties.", 
       fill = "Significance") + theme_minimal() +
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
        legend.text = element_text(size = 7)) 

###################################
## 2018 - 2022 SIGNIFICANT PLOTS ##
###################################
counties_2018 <- counties_all %>%
  filter(epoch == '2018-2022')

plot_2018 <- ggplot() +
  geom_sf(data = national_shp, fill = 'bisque3', size = 0.001) +
  geom_sf(data = counties_2018, aes(fill = factor(significant)), color = NA) +
  scale_fill_manual(values = c('0' = 'bisque3', '1' = 'tomato3'),
                    labels = c('0' = 'Not significant', '1' = 'Significant'),
                    name   = 'Significance') +
  facet_wrap(~ variable_label, ncol = 4) +
  labs(title = " ",
       subtitle = "(C) 2018 - 2022: Spatial extent of each variable's significant association with PE mortality in US counties.", 
       fill = "Significance") + theme_minimal() +
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
        legend.text = element_text(size = 7)) 

combined_sign_plots <- ggarrange(plot_2005, plot_2011, plot_2018, ncol = 1,
                            nrow = 3, common.legend = TRUE, legend = 'bottom')

path = file.path(folder, 'figures', 'MGWR_sign_plots.png')
png(path, units="in", width=8, height=6, res=720)
print(combined_sign_plots)
dev.off()

#######################
## SIGNIFICANT PLOTS ##
#######################

################################
## 1. Significant 2005 - 2010 ##
################################
counties_2005 <- counties_all %>%
  filter(significant == 1)

counties_2005_df <- counties_2005 %>%
  sf::st_drop_geometry() %>%
  dplyr::select(FIPS, STATE_NAME, variable_label) %>%
  dplyr::distinct(FIPS, variable_label, .keep_all = TRUE)   

sig_totals_by_state <- counties_2005_df %>%
  group_by(STATE_NAME, variable_label) %>%
  summarise(total_significant = n(), .groups = 'drop') %>%
  arrange(STATE_NAME, variable_label)

### Get the total number of counties for each state ###
census_counties <- get_decennial(geography = 'county',
  variables = 'P1_001N', year = 2020) %>%
  mutate(STATE_NAME = sub('.*, ', '', NAME)) %>%
  group_by(STATE_NAME) %>%
  summarise(total_counties = n(), .groups = 'drop')

sig_totals_by_state <- sig_totals_by_state %>%
  left_join(census_counties, by = 'STATE_NAME') %>%
  mutate(pct_significant = round(total_significant / total_counties * 100, 2))

# Remove Delaware since it always return 100 as it only has 3 counties
sig_totals_by_state <- sig_totals_by_state %>%
  filter(STATE_NAME != 'Delaware')

top5_by_variable <- sig_totals_by_state %>%
  group_by(variable_label) %>%
  slice_max(order_by = pct_significant, n = 5) %>%
  arrange(variable_label, desc(pct_significant))


bar_plot_2010 <- ggplot(top5_by_variable, aes(x = reorder(STATE_NAME, 
  pct_significant), y = pct_significant, fill = STATE_NAME)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  facet_wrap(~ variable_label, scales = 'free_y', ncol = 4) +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Percentage of Counties (%)",
       title = "Top 5 States where independent varibales are statistically significant",
       subtitle = "(A) 2005 - 2010 Period") +
  geom_text(aes(label = paste0(round(pct_significant), "%")), 
            vjust = 0.5, hjust = -0.1, size = 2) +
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
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 119))


################################
## 2. Significant 2011 - 2017 ##
################################
counties_2011 <- counties_2011 %>%
  filter(significant == 1)

counties_2011_df <- counties_2011 %>%
  sf::st_drop_geometry() %>%
  dplyr::select(FIPS, STATE_NAME, variable_label) %>%
  dplyr::distinct(FIPS, variable_label, .keep_all = TRUE)   

sig_totals_by_state_2011 <- counties_2011_df %>%
  group_by(STATE_NAME, variable_label) %>%
  summarise(total_significant = n(), .groups = 'drop') %>%
  arrange(STATE_NAME, variable_label)

sig_totals_by_state_2011 <- sig_totals_by_state_2011 %>%
  left_join(census_counties, by = 'STATE_NAME') %>%
  mutate(pct_significant = round(total_significant / total_counties * 100, 2))

sig_totals_by_state_2011 <- sig_totals_by_state_2011 %>%
  filter(STATE_NAME != 'Delaware')

top5_by_variable_2011 <- sig_totals_by_state_2011 %>%
  group_by(variable_label) %>%
  slice_max(order_by = pct_significant, n = 5) %>%
  arrange(variable_label, desc(pct_significant))

bar_plot_2017 <- ggplot(top5_by_variable_2011, aes(x = reorder(STATE_NAME, 
  pct_significant), y = pct_significant, fill = STATE_NAME)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  facet_wrap(~ variable_label, scales = 'free_y', ncol = 4) +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Percentage of Counties (%)", title = " ",
       subtitle = "(B) 2011 - 2017 Period") +
  geom_text(aes(label = paste0(round(pct_significant), "%")), 
            vjust = 0.5, hjust = -0.1, size = 2) +
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
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 114))

################################
## 3. Significant 2018 - 2022 ##
################################
counties_2018 <- counties_2018 %>%
  filter(significant == 1)

counties_2018_df <- counties_2018 %>%
  sf::st_drop_geometry() %>%
  dplyr::select(FIPS, STATE_NAME, variable_label) %>%
  dplyr::distinct(FIPS, variable_label, .keep_all = TRUE)   

sig_totals_by_state_2018 <- counties_2018_df %>%
  group_by(STATE_NAME, variable_label) %>%
  summarise(total_significant = n(), .groups = 'drop') %>%
  arrange(STATE_NAME, variable_label)

sig_totals_by_state_2018 <- sig_totals_by_state_2018 %>%
  left_join(census_counties, by = 'STATE_NAME') %>%
  mutate(pct_significant = round(total_significant / total_counties * 100, 2))

sig_totals_by_state_2018 <- sig_totals_by_state_2018 %>%
  filter(STATE_NAME != 'Delaware')

top5_by_variable_2018 <- sig_totals_by_state_2018 %>%
  group_by(variable_label) %>%
  slice_max(order_by = pct_significant, n = 5) %>%
  arrange(variable_label, desc(pct_significant))

bar_plot_2018 <- ggplot(top5_by_variable_2018, aes(x = reorder(STATE_NAME, 
  pct_significant), y = pct_significant, fill = STATE_NAME)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  facet_wrap(~ variable_label, scales = 'free_y', ncol = 4) +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Percentage of Counties (%)", title = " ",
       subtitle = "(C) 2018 - 2022 Period") +
  geom_text(aes(label = paste0(round(pct_significant), "%")), 
            vjust = 0.5, hjust = -0.1, size = 2) +
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
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 114))

combined_plots_sig <- ggarrange(bar_plot_2010, bar_plot_2017, bar_plot_2018, 
                                ncol = 1, nrow = 3)

path = file.path(folder, 'figures', 'significant_bar_plots.png')
png(path, units="in", width=9, height=5, res=720)
print(combined_plots_sig)
dev.off()
  


