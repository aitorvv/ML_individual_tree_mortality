#------------------------------------------------------------------------------------------#
####                               Performance graphs                                   ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 20/06/2023                              #
#                              Last modification: 23/02/2024                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library(openxlsx)

setwd('ML_individual_tree_mortality/')


#### Get points data ####

# get my points
original_points <- read.xlsx('1_data/0_raw/Koordinaten.xlsx', sep = ',')

# get ID
original_points$INVENTORY_ID <- substr(original_points$VF, 1, 3)

# on that case I just need 1 coordinate per plot, so I will remove the rest
original_points <- original_points[!duplicated(original_points$INVENTORY_ID), ]

# transform to WGS84
my_points <- st_as_sf(original_points, coords = c("Longitude", "Latitude"), 
                      crs = 4326, # WGS84
                      agr = "constant")


#### Get shape data ####

# shape all Europe
shp_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 1, # big regions
                                 year = 2021,
                                 crs = 4326) # WGS84

# shape Germany
shp_de <- shp_0[shp_0$CNTR_CODE == 'DE', ]

# check it
shp_de %>% 
  ggplot() +
  geom_sf()

# plot it cleaner
shp_de %>% 
  ggplot() +
  geom_sf(size = 0.2, color = "blue") + # border line
  scale_x_continuous(limits = c(5, 15)) +
  scale_y_continuous(limits = c(46, 56)) +
  labs(
    title = "Site locations",
    #subtitle = "Annual % of change, 2020 vs 2019",
    #caption = "Data: Eurostat tec00115"
  ) +
  theme_void() # skip borders of the plot

# get small regions
shp_regions  <- get_eurostat_geospatial(
  resolution = 10,
  nuts_level = 2, # medium regions
  year = 2021,
  crs = 4326) # WGS84

# get regions from Germany
shp_de_regions <- shp_regions[shp_regions$CNTR_CODE == 'DE', ]

# plot German regions
shp_de_regions %>% 
  ggplot() +
  geom_sf(size = 0.2, color = "blue") + # border line
  scale_x_continuous(limits = c(5, 15)) +
  scale_y_continuous(limits = c(46, 56)) +
  labs(
    title = "Site locations",
    #subtitle = "Annual % of change, 2020 vs 2019",
    #caption = "Data: Eurostat tec00115"
  ) +
  theme_void() # skip borders of the plot


#### Plot points and map ####

# baseline map
base <- ggplot(shp_de) +
  geom_sf(size = 0.2, color = "black", fill = 'lightgray') + # border line
  geom_sf_text(aes(label = NAME_LATN), size = 5, family = 'sans', # text for each plot
               nudge_x = 0, nudge_y = 0, check_overlap = TRUE) + # distance between point and label
  geom_sf(data = my_points, size = 3, shape = 23, fill = "darkred") + # points
  #scale_x_continuous(limits = c(5, 15)) +
  #scale_y_continuous(limits = c(46, 56)) +
  labs(title = "Long-term experimental plots location in Germany" # title
  #subtitle = "Annual % of change, 2020 vs 2019",
  #caption = "Data: Eurostat tec00115"
  ) +
  theme_void() + # skip borders of the plot
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))  # center title
  # xlab("Longitude") + ylab("Latitude")  # x and y labels

base

# extra map
plots <- ggplot(data = shp_de_regions) + # regions shp
  geom_sf() +
  geom_sf_text(aes(label = NAME_LATN), size = 5, family = 'sans', # text for each plot
               nudge_x = 0, nudge_y = 0) + # distance between point and label
  geom_sf(data = my_points, size = 3, shape = 23, fill = 'darkred') + # add points data
  geom_sf_text(data = my_points, aes(label = INVENTORY_ID), size = 4.5, 
               family = 'sans', color = 'darkred', fontface = 'bold', # text for each plot
               nudge_x = 0, nudge_y = 0.1, check_overlap = TRUE) + # distance between point and label
  coord_sf(xlim = c(min(original_points$Longitude) - 0.2, max(original_points$Longitude) + 0.2), # x limits
           ylim = c(min(original_points$Latitude), max(original_points$Latitude) + 0.1)) + # y limits
  #annotate("text", x = min(original_points$Longitude) + 0.5, y = max(original_points$Latitude), 
  #         label = "Study sites", size = 4, family = 'sans', fontface = 'bold') + # text
  labs(title = "Detailed plots location in Bavaria") + # title
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15)) + # center title
  xlab("Longitude") + ylab("Latitude")  # x and y labels
  #theme_void() 
#  theme(panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", size = 0.5), 
#        panel.background = element_rect(fill = "aliceblue"), 
#        panel.border = element_rect(fill = NA))

plots

# arrow location
arrow <- data.frame(x1 = 11.5, x2 = 22, y1 = 4.75, y2 = 10)

# both maps
full_map <- ggplot() +
  coord_equal(xlim = c(0, 40), ylim = c(0, 20), expand = FALSE) + # box limits
  annotation_custom(ggplotGrob(base), xmin = 0, xmax = 20, 
                    ymin = 0, ymax = 20) + # base map and size inside box limits
  annotation_custom(ggplotGrob(plots), xmin = 20, xmax = 40, 
                    ymin = 0, ymax = 20) + # plots map and size inside box limits
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrow, 
               arrow = arrow(), lineend = "round") + # arrow
  theme_void()

full_map

# save graph
g_name <- '9.0_location_map'
my_path <- paste('3_figures/final_figures/', g_name, '.png', sep = '')
ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
