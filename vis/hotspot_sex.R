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
contiguous_states <- c(
  "AL","AR","AZ","CA","CO","CT","DE","FL","GA","IA","ID","IL","IN",
  "KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND",
  "NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD",
  "TN","TX","UT","VA","VT","WA","WI","WV","WY")
usa_outline <- st_read(file.path(folder, '..', 'data', 'raw', 'geodata', 
                                 'US_national_outline.shp'))
usa_outline <- st_transform(usa_outline, crs = 5070)

#############
## 1. MALE ##
#############
male_pre <- st_read(file.path(folder, '..', 'results', 'hotspot', 'shps', 
                             'sex', 'male_pre_2015_gibin.shp'))

male_post <- st_read(file.path(folder, '..', 'results', 'hotspot', 'shps', 
                              'sex', 'male_after_2015_gibin.shp'))

male_pre <- male_pre[, c('STATEABBRV', 'Gi_Bin', 'geometry')]
male_pre$epoch <- "2005 to 2015"

male_post <- male_post[, c('STATEABBRV', 'Gi_Bin', 'geometry')]
male_post$epoch <- "2016 to 2022"

male_gdf <- rbind(male_pre, male_post)

male_gdf <- male_gdf %>%
  mutate(
    gi_label = case_when(
      Gi_Bin ==  3 ~ "Hot Spot (99%)",
      Gi_Bin ==  2 ~ "Hot Spot (95%)",
      Gi_Bin ==  1 ~ "Hot Spot (90%)",
      Gi_Bin ==  0 ~ "Not Significant",
      Gi_Bin == -1 ~ "Cold Spot (90%)",
      Gi_Bin == -2 ~ "Cold Spot (95%)",
      Gi_Bin == -3 ~ "Cold Spot (99%)",
    )
  )

male_gdf <- male_gdf[male_gdf$STATEABBRV %in% contiguous_states, ]
male_gdf <- st_transform(male_gdf, crs = 5070)
male_gdf$gi_label <- factor(male_gdf$gi_label,
   levels = c('Not Significant', 'Cold Spot (90%)', 'Cold Spot (95%)', 
              'Cold Spot (99%)', 'Hot Spot (90%)', 'Hot Spot (95%)',
              'Hot Spot (99%)'),
   labels = c('Not Significant', 'Cold Spot (90%)', 'Cold Spot (95%)', 
              'Cold Spot (99%)', 'Hot Spot (90%)', 'Hot Spot (95%)',
              'Hot Spot (99%)'))

male_hotspot <- ggplot() + 
geom_sf(data = usa_outline, linewidth = 0.2, fill = NA, color = "black") +
  geom_sf(data = male_gdf, aes(fill = gi_label),  color = NA, linetype = 0,
          inherit.aes = FALSE) +
  scale_fill_viridis_d(direction = 1, na.value = "grey98",
                       labels = function(x) ifelse(is.na(x), "No Data", x)) +
  labs(title = "(A) Male Hotspot Analysis Maps.",
       subtitle = "PE mortality count hotspot analysis before and after 2015 for continental US counties (excluding counties from Alaska and Hawaii).", 
       fill = "Hotspot") + theme_minimal() +
  theme(legend.position = 'bottom',
        plot.margin = margin(0, 0, 0, 0),              
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.line.x  = element_line(size = 0.15),
        axis.line.y  = element_line(size = 0.15),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7)) +
  coord_sf(expand = FALSE) +
  guides(fill = guide_legend(ncol = 8)) +
  facet_wrap( ~ epoch, ncol = 2)

###############
## 2. FEMALE ##
###############
female_pre <- st_read(file.path(folder, '..', 'results', 'hotspot', 'shps', 
                              'sex', 'female_pre_2015_gibin.shp'))

female_post <- st_read(file.path(folder, '..', 'results', 'hotspot', 'shps', 
                               'sex', 'female_after_2015_gibin.shp'))

female_pre <- female_pre[, c('STATEABBRV', 'Gi_Bin', 'geometry')]
female_pre$epoch <- "2005 to 2015"

female_post <- female_post[, c('STATEABBRV', 'Gi_Bin', 'geometry')]
female_post$epoch <- "2016 to 2022"

female_gdf <- rbind(female_pre, female_post)
female_gdf <- female_gdf %>%
  mutate(
    gi_label = case_when(
      Gi_Bin ==  3 ~ "Hot Spot (99%)",
      Gi_Bin ==  2 ~ "Hot Spot (95%)",
      Gi_Bin ==  1 ~ "Hot Spot (90%)",
      Gi_Bin ==  0 ~ "Not Significant",
      Gi_Bin == -1 ~ "Cold Spot (90%)",
      Gi_Bin == -2 ~ "Cold Spot (95%)",
      Gi_Bin == -3 ~ "Cold Spot (99%)",
    )
  )

female_gdf <- female_gdf[female_gdf$STATEABBRV %in% contiguous_states, ]
female_gdf <- st_transform(female_gdf, crs = 5070)
female_gdf$gi_label <- factor(female_gdf$gi_label,
    levels = c('Not Significant', 'Cold Spot (90%)', 'Cold Spot (95%)', 
               'Cold Spot (99%)', 'Hot Spot (90%)', 'Hot Spot (95%)',
               'Hot Spot (99%)'),
    labels = c('Not Significant', 'Cold Spot (90%)', 'Cold Spot (95%)', 
               'Cold Spot (99%)', 'Hot Spot (90%)', 'Hot Spot (95%)',
               'Hot Spot (99%)'))
female_hotspot <- ggplot() + 
  geom_sf(data = usa_outline, linewidth = 0.2, fill = NA, color = "black") +
  geom_sf(data = male_gdf, aes(fill = gi_label),  color = NA, linetype = 0) +
  scale_fill_viridis_d(direction = 1, na.value = "grey98",
                       labels = function(x) ifelse(is.na(x), "No Data", x)) +
  labs(title = "(B) Female Hotspot Analysis Maps.",
       subtitle = "PE mortality count hotspot analysis before and after 2015 for continental US counties (excluding counties from Alaska and Hawaii).", 
       fill = "Hotspot") + theme_minimal() +
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
  guides(fill = guide_legend(ncol = 8)) +
  facet_wrap( ~ epoch, ncol = 2)

###############################
## 3. Male Hotspots pre-2015 ##
###############################
male_pre_hot <- male_gdf %>%
  mutate(gi_label_clean = toupper(trimws(gi_label))) %>%  
  filter(gi_label_clean == "HOT SPOT (99%)", epoch == "2005 to 2015") %>%
  st_drop_geometry() %>%
  group_by(STATEABBRV) %>%
  summarise(totals = n()) %>%
  ungroup()

male_pre_10 <- male_pre_hot %>%
  arrange(desc(totals)) %>%  # 
  slice_head(n = 10) 

state_population_2015 <- data.frame(
  STATEABBRV = c("TX","KS","MO","MS","AR","OK","NE","LA","TN","CO"),
  population_2015 = c(27429639, 2911641, 6083672, 2988474, 2977853, 3911338,
    1896190, 4670724, 6600299, 5456574))

male_pre_10 <- male_pre_10 %>%
  left_join(state_population_2015, by = "STATEABBRV")

male_pre_10 <- male_pre_10 %>%
  mutate(per_100k = (totals / population_2015)*100000)

male_pre_10$STATEABBRV <- factor(male_pre_10$STATEABBRV,
   levels = c('CO', 'TN', 'TX', 'LA', 'MO', 'OK', 'AR', 'MS', 'NE', 'KS'),
   labels = c('Colorado', 'Tennessee', 'Texas', 'Louisiana', 'Missouri',
              'Oklahoma', 'Arkansas', 'Mississippi', 'Nebraska', 'Kansas'))

male_pre_top10 <- ggplot(male_pre_10, aes(x = STATEABBRV, y = per_100k, fill = STATEABBRV)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Counts per 100,000 people",
       title = "(C) Top 10 hotspot States where PE mortality counts are highest.",
       subtitle = "Male 2005 to 2015") +
  geom_text(aes(label = sprintf("%.1f", per_100k)), size = 2, hjust = -0.1) +
  theme(
    legend.position = 'none',
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
  scale_x_discrete(expand = c(0, 0.15)) + 
  scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 3.5))

################################
## 4. Male Hotspots post-2015 ##
################################
male_post_hot <- male_gdf %>%
  mutate(gi_label_clean = toupper(trimws(gi_label))) %>%  
  filter(gi_label_clean == "HOT SPOT (99%)", epoch == "2016 to 2022") %>%
  st_drop_geometry() %>%
  group_by(STATEABBRV) %>%
  summarise(totals = n()) %>%
  ungroup()

male_post_10 <- male_post_hot %>%
  arrange(desc(totals)) %>%  # 
  slice_head(n = 10)

state_population_2023 <- data.frame(
  STATEABBRV = c("TX","MO","KS","MS","AR","AL","OK","NE","LA","CO"),
  population_2023 = c(30503301, 6177957, 2954832, 2940057, 3040207, 5097641,
                      4053824, 2002052, 4590241, 5877610))

male_post_10 <- male_post_10 %>%
  left_join(state_population_2023, by = "STATEABBRV")

male_post_10 <- male_post_10 %>%
  mutate(per_100k = (totals / population_2023)*100000)

male_post_10$STATEABBRV <- factor(male_post_10$STATEABBRV,
   levels = c('TX', 'CO', 'LA', 'AL', 'OK', 'MO', 'AR', 'NE', 'MS', 'KS'),
   labels = c('Texas', 'Colorado', 'Louisiana', 'Alabama', 'Oklahoma',
              'Missouri', 'Arkansas', 'Nebraska', 'Mississippi', 'Kansas'))

male_post_top10 <- ggplot(male_post_10, aes(x = STATEABBRV, y = per_100k, 
                                            fill = STATEABBRV)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Counts per 100,000 people",
       title = " ",
       subtitle = "Male 2016 to 2022") +
  geom_text(aes(label = sprintf("%.1f", per_100k)), size = 2, hjust = -0.1) +
  theme(
    legend.position = 'none',
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
  scale_x_discrete(expand = c(0, 0.15)) + 
  scale_y_continuous(expand = c(0, 0),
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 3))

#################################
## 5. Female Hotspots pre-2015 ##
#################################
female_pre_hot <- female_gdf %>%
  mutate(gi_label_clean = toupper(trimws(gi_label))) %>%  
  filter(gi_label_clean == "HOT SPOT (99%)", epoch == "2005 to 2015") %>%
  st_drop_geometry() %>%
  group_by(STATEABBRV) %>%
  summarise(totals = n()) %>%
  ungroup()

female_pre_10 <- female_pre_hot %>%
  arrange(desc(totals)) %>%  # 
  slice_head(n = 10) 

state_population_2015 <- data.frame(
  STATEABBRV = c("TX","MO","KS","MS","AR","IA","NE","OK","LA","CO"),
  population_2015 = c(27429639, 6083672, 2911641, 2988474, 2977853, 3120960,
    1896190, 3911338, 4670724, 5456574))

female_pre_10 <- female_pre_10 %>%
  left_join(state_population_2015, by = "STATEABBRV")

female_pre_10 <- female_pre_10 %>%
  mutate(per_100k = (totals / population_2015)*100000)

female_pre_10$STATEABBRV <- factor(female_pre_10$STATEABBRV,
    levels = c('TX', 'CO', 'LA', 'OK', 'MO', 'IA', 'AR', 'MS', 'KS', 'NE'),
    labels = c('Texas', 'Colorado', 'Louisiana', 'Oklahoma', 'Missouri',
               'Iowa', 'Arkansas', 'Mississippi', 'Kansas', 'Nebraska'))

female_pre_top10 <- ggplot(female_pre_10, aes(x = STATEABBRV, y = per_100k, 
                                            fill = STATEABBRV)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Counts per 100,000 people",
       title = " ",
       subtitle = "Female 2005 to 2015") +
  geom_text(aes(label = sprintf("%.1f", per_100k)), size = 2, hjust = -0.1) +
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
  scale_x_discrete(expand = c(0, 0.15)) + 
  scale_y_continuous(expand = c(0, 0),
    labels = function(y)format(y, scientific = FALSE), limit = c(0, 4))

##################################
## 6. Female Hotspots post-2015 ##
##################################
female_post_hot <- female_gdf %>%
  mutate(gi_label_clean = toupper(trimws(gi_label))) %>%  
  filter(gi_label_clean == "HOT SPOT (99%)", epoch == "2016 to 2022") %>%
  st_drop_geometry() %>%
  group_by(STATEABBRV) %>%
  summarise(totals = n()) %>%
  ungroup()

female_post_10 <- female_post_hot %>%
  arrange(desc(totals)) %>%  # 
  slice_head(n = 10)

state_population_2023 <- data.frame(
  STATEABBRV = c("TX","MO","AR","OK","KS","MS","NE","LA","AL","CO"),
  population_2023 = c(30503301, 6177957, 3040207, 4053824, 2954832, 2940057,
    2002052, 4590241, 5097641, 5877610))

female_post_10 <- female_post_10 %>%
  left_join(state_population_2023, by = "STATEABBRV")

female_post_10 <- female_post_10 %>%
  mutate(per_100k = (totals / population_2023)*100000)

female_post_10$STATEABBRV <- factor(female_post_10$STATEABBRV,
   levels = c('TX', 'CO', 'AL', 'LA', 'MO', 'OK', 'MS', 'KS', 'AR', 'NE'),
   labels = c('Texas', 'Colorado', 'Alabama', 'Louisiana', 'Missouri',
              'Oklahoma', 'Mississippi', 'Kansas', 'Arkansas', 'Nebraska'))

female_post_top10 <- ggplot(female_post_10, aes(x = STATEABBRV, y = per_100k, 
                                            fill = STATEABBRV)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Counts per 100,000 people",
       title = " ",
       subtitle = "Female 2016 to 2022") +
  geom_text(aes(label = sprintf("%.1f", per_100k)), size = 2, hjust = -0.1) +
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
  scale_x_discrete(expand = c(0, 0.15)) + 
  scale_y_continuous(expand = c(0, 0),
    labels = function(y)format(y, scientific = FALSE), limit = c(0, 3.4))

map_plots <- ggarrange(male_hotspot, female_hotspot, nrow = 2, align = c('v'),
                       common.legend = TRUE, legend='bottom')

significant_plots <- ggarrange(male_pre_top10, male_post_top10, 
                               female_pre_top10, female_post_top10, ncol = 4,
                               nrow = 1, heights = c(1, 1))

combined_plots <- ggarrange(map_plots, significant_plots, ncol = 1,
                            nrow = 2, heights = c(4, 1), align = c('v'))

path = file.path(folder, 'figures', 'sex_hotspots.png')
png(path, units="in", width=8.5, height=7, res=300)
print(combined_plots)
dev.off()





