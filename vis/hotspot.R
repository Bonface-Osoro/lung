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
  "TN","TX","UT","VA","VT","WA","WI","WV","WY"
)
#############
## GENERAL ##
#############
pre_gen <- st_read(file.path(folder, '..', 'results', 'hotspot', 'shps', 
                              'general', 'gen_pre_2015_gibin.shp'))

post_gen <- st_read(file.path(folder, '..', 'results', 'hotspot', 'shps', 
                             'general', 'gen_after_2015_gibin.shp'))

pre_gen <- pre_gen[, c('STATEABBRV', 'Gi_Bin', 'geometry')]
pre_gen$epoch <- "2005 to 2015"

post_gen <- post_gen[, c('STATEABBRV', 'Gi_Bin', 'geometry')]
post_gen$epoch <- "2016 to 2022"

gen_gdf <- rbind(pre_gen, post_gen)

gen_gdf <- gen_gdf %>%
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

gen_gdf <- gen_gdf[gen_gdf$STATEABBRV %in% contiguous_states, ]
gen_gdf <- st_transform(gen_gdf, crs = 5070)
gen_gdf$gi_label <- factor(gen_gdf$gi_label,
  levels = c('Not Significant', 'Cold Spot (90%)', 'Cold Spot (95%)', 
             'Cold Spot (99%)', 'Hot Spot (90%)', 'Hot Spot (95%)',
             'Hot Spot (99%)'),
  labels = c('Not Significant', 'Cold Spot (90%)', 'Cold Spot (95%)', 
             'Cold Spot (99%)', 'Hot Spot (90%)', 'Hot Spot (95%)',
             'Hot Spot (99%)'))

gen_hotspot <- ggplot() + 
  geom_sf(data = gen_gdf, aes(fill = gi_label), linewidth = 0.001) +
  scale_fill_viridis_d(direction = 1, na.value = "grey80",
    labels = function(x) ifelse(is.na(x), "No Data", x)) +
  labs(title = "(A) Hotspot Analysis Maps.",
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
  guides(fill = guide_legend(ncol = 6)) +
  facet_wrap( ~ epoch, ncol = 2, nrow = 2) 

##########################
## 1. Hotspots pre-2015 ##
##########################
gen_hot99 <- gen_gdf %>%
  mutate(gi_label_clean = toupper(trimws(gi_label))) %>%  
  filter(gi_label_clean == "HOT SPOT (99%)", epoch == "2005 to 2015") %>%
  st_drop_geometry() %>%
  group_by(STATEABBRV) %>%
  summarise(totals = n()) %>%
  ungroup()

top10_gen <- gen_hot99 %>%
  arrange(desc(totals)) %>%  # 
  slice_head(n = 10) 

state_population_2015 <- data.frame(
  STATEABBRV = c("TX","MO","KS","NE","MS","AR","OK","TN","LA","IL"),
  population_2015 = c(27429639, 6083672, 2911641, 1896190, 2988474,
    2977853, 3911338, 6600299, 4670724, 12859995))

top10_gen <- top10_gen %>%
  left_join(state_population_2015, by = "STATEABBRV")

top10_gen <- top10_gen %>%
  mutate(per_100k = (totals / population_2015)*100000)

top10_gen$STATEABBRV <- factor(top10_gen$STATEABBRV,
  levels = c('IL', 'TX', 'TN', 'LA', 'MO', 'OK', 'AR', 'MS', 'KS', 'NE'),
  labels = c('Illinois', 'Texas', 'Tennessee', 'Louisiana', 'Missouri',
             'Oklahoma', 'Arkansas', 'Mississippi', 'Kansas', 'Nebraska'))

pre_2015_top10 <- ggplot(top10_gen, aes(x = STATEABBRV, y = per_100k, fill = STATEABBRV)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Counts per 100,000 people",
       title = "(B) Top 10 hotspot States where PE mortality counts are highest.",
       subtitle = "2005 to 2015") +
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
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 5))

###########################
## 2. Hotspots post-2015 ##
###########################
gen_post_hot99 <- gen_gdf %>%
  mutate(gi_label_clean = toupper(trimws(gi_label))) %>%  
  filter(gi_label_clean == "HOT SPOT (99%)", epoch == "2016 to 2022") %>%
  st_drop_geometry() %>%
  group_by(STATEABBRV) %>%
  summarise(totals = n()) %>%
  ungroup()

post_top10_gen <- gen_post_hot99 %>%
  arrange(desc(totals)) %>%  # 
  slice_head(n = 10)

state_population <- data.frame(
  STATEABBRV = c("AL","AR","GA","KY","LA","MO","MS","OK","TN","TX"),
  population_2023 = c(5097641, 3040207, 11029227, 4512310, 4590241, 6177957,
                      2940057, 4053824, 7126489, 30503301))

post_top10_gen <- post_top10_gen %>%
  left_join(state_population, by = "STATEABBRV")

post_top10_gen <- post_top10_gen %>%
  mutate(per_100k = (totals / population_2023)*100000)

post_top10_gen$STATEABBRV <- factor(post_top10_gen$STATEABBRV,
   levels = c('TX', 'GA', 'MO', 'TN', 'AL', 'KY', 'LA', 'OK', 'AR', 'MS'),
   labels = c('Texas', 'Georgia', 'Missouri', 'Tennessee', 'Alabama', 'Kentucky',
              'Louisiana', 'Oklahoma', 'Arkansas', 'Mississippi' ))

post_2015_top10 <- ggplot(post_top10_gen, aes(x = STATEABBRV, y = per_100k, fill = STATEABBRV)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  scale_fill_viridis_d(direction = 1) + theme_minimal() +
  labs(x = NULL,
       y = "Counts per 100,000 people",
       title = " ",
       subtitle = "2016 to 2022") +
  geom_text(aes(label = sprintf("%.1f", per_100k)), size = 2, hjust = -0.1) +
  theme(legend.position = 'bottom',
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
  labels = function(y)format(y, scientific = FALSE), limit = c(0, 3.2))

significant_plots <- ggarrange(pre_2015_top10, post_2015_top10, ncol = 2,
                               nrow = 1, heights = c(1, 1))

combined_plots <- ggarrange(gen_hotspot, significant_plots, ncol = 1,
                            nrow = 2, heights = c(3.5, 1.5))


path = file.path(folder, 'figures', 'overall_hotspots.png')
png(path, units="in", width=7, height=5, res=300)
print(combined_plots)
dev.off()




