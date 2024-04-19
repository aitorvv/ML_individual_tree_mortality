#------------------------------------------------------------------------------------------#
####                              Graphs: mortality rate                                ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 20/06/2023                              #
#                              Last modification: 22/02/2024                               #
#------------------------------------------------------------------------------------------#

library(tidyverse)
library(ggplot2)

setwd('ML_individual_tree_mortality/')

df_original <- read.csv('1_data/tmp_DEN/2_clima/df_complete_r33.csv')

# variable for tree status
df_original$status <- ifelse(df_original$removed == 'no', 'alive', ifelse(
  df_original$removed == 'yes' & df_original$dead == 'yes', 'dead', 'thinned'
))

# variables for plot means
df_original$tmp_dead <- ifelse(df_original$dead == 'yes', 1, 0)
df_original$tmp_thinned <- ifelse(df_original$thinned == 'yes', 1, 0)
df_original$tmp_alive <- ifelse(df_original$removed == 'no', 1, 0)
df_original$tmp_all <- ifelse(df_original$removed == 'yes', 1, 1)

# group by plot and year
df_sum <- plyr::ddply(df_original, c('INVENTORY_ID', 'PLOT_ID', 'year', 'AGE'), summarise, 
                # CODES
                INVENTORY_ID = unique(INVENTORY_ID),
                PLOT_ID = unique(PLOT_ID), 
                SI_100 = unique(SI_100),
                # variables
                N_dead = sum(tmp_dead, na.rm = TRUE),
                N_thinned = sum(tmp_thinned, na.rm = TRUE),
                N_alive = sum(tmp_alive, na.rm = TRUE),
                N = sum(tmp_all, na.rm = TRUE)
)

# calculate mortality rate
df_sum$mortality_rate <- (df_sum$N_dead/df_sum$N)*100

# check mortality rate summary
m_rates <- tapply(df_sum$mortality_rate, df_sum$PLOT_ID, summary)
means <- medians <- tibble()
for(plot in 1:length(m_rates)){
  medians <- rbind(medians, m_rates[[plot]][3]) # median
  means <- rbind(means, m_rates[[plot]][4]) # mean
}
summary(medians) # get median of medians
summary(means) # get mean of means
#summary(df_sum$mortality_rate)


#### Graph results: mortality rate among inventories ####

# https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2

# mortality rate per year and inventory
g <- ggplot(df_sum, aes(x = year, y = mortality_rate, fill = PLOT_ID)) + # data
        geom_bar(position = 'dodge', stat = 'identity') + # position
        theme_minimal() + # graph theme
        theme(plot.title = element_text(hjust = 0.5),
              legend.title = element_text(hjust = 0.5)) + # center title
        labs(title =  'Mortality rate evolution among field measurements',
             x = 'Measurement (year)', y = 'Mortality rate (%)') + # labels        
        scale_fill_manual('Plot', values = gray.colors(97)) + # set color in grayscale
        facet_wrap(~INVENTORY_ID) # one graph per inventory

g


# no legend
g <- ggplot(df_sum, aes(x = year, y = mortality_rate, fill = PLOT_ID)) + # data
  geom_bar(position = 'dodge', stat = 'identity') + # position
  theme_minimal() + # graph theme
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5),
        legend.position = 'none') + # center title
  labs(title =  'Mortality rate evolution among field measurements',
       x = 'Measurement (year)', y = 'Mortality rate (%)') + # labels        
  scale_fill_manual('Plot', values = gray.colors(97)) + # set color in grayscale
  facet_wrap(~INVENTORY_ID, scales = 'free') # one graph per inventory

g

# save graph
g_name <- 'mortality_rate_per_site'
my_path <- paste('3_figures/tmp_figures/9.2_mortality_rates/', g_name, '.png', sep = '')
ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)

# COMMENTS:
# That graphs are not the best, as EUR i.e. show 100% mortality rate in 1976 but
# that is because thinned was applied and the rest of trees were not measured


#### Graph results: mortality rate among dbh classes ####

# inventory and dbh classes
ggplot(df_original, aes(x = dbh, fill = dead, color = thinned)) + # data
  geom_histogram(binwidth = 0.5) +
  theme_minimal() + # graph theme
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15, hjust = 0.5),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  labs(title =  'Mortality among dbh classes for each experimental site',
       x = 'Tree dbh (cm)', y = 'Trees per plot') + # labels        
  scale_fill_manual('Dead tree', values = gray.colors(2, start = 0.5, end = 0.8)) + # set color in grayscale
  scale_color_manual('Thinned tree', values = c('green', 'red')) +
  facet_wrap(~INVENTORY_ID, scales = 'free') # one graph per inventory


# the same in grayscale
g <- ggplot(df_original, aes(x = dbh, fill = status)) + # data
  geom_histogram(binwidth = 0.5) +
  theme_minimal() + # graph theme
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15, hjust = 0.5),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  labs(title =  'Mortality among dbh classes for each experimental site',
       x = 'Tree dbh (cm)', y = 'Number of trees') + # labels       
  scale_fill_manual('Tree status', values = gray.colors(3, start = 0.8, end = 0.1)) + # set color in grayscale
  facet_wrap(~INVENTORY_ID, scales = 'free') # one graph per inventory

g

# save graph
g_name <- 'mortality_per_dbh_and_site'
my_path <- paste('3_figures/tmp_figures/9.2_mortality_rates/', g_name, '.png', sep = '')
ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


# just dbh classes, skipping inventory segmentation
g <- ggplot(df_original, aes(x = dbh, fill = status)) + # data
  geom_histogram(binwidth = 0.5) +
  theme_minimal() + # graph theme
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15, hjust = 0.5),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  labs(title =  'Mortality among dbh classes',
       x = 'Tree dbh (cm)', y = 'Number of trees') + # labels       
  scale_fill_manual('Tree status', values = gray.colors(3, start = 0.9, end = 0.1))  # set color in grayscale

g

# save graph
g_name <- 'mortality_per_dbh'
my_path <- paste('3_figures/tmp_figures/9.2_mortality_rates/', g_name, '.png', sep = '')
ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


# both previous graphs together
df_duplicated <- df_original
df_duplicated$INVENTORY_ID <- 'All experimental sites'
df_duplicated <- rbind(df_duplicated, df_original)
g <- ggplot(df_duplicated, aes(x = dbh, fill = status)) + # data
  geom_histogram(binwidth = 0.5) +
  theme_minimal() + # graph theme
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15, hjust = 0.5),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  labs(title =  'Mortality among dbh classes for each experimental site',
       x = 'Tree dbh (cm)', y = 'Number of trees') + # labels       
  scale_fill_manual('Tree status', values = gray.colors(3, start = 0.8, end = 0.1)) + # set color in grayscale
  facet_wrap(~INVENTORY_ID, scales = 'free') # one graph per inventory

g

# save graph
g_name <- 'mortality_per_dbh_ALL'
my_path <- paste('3_figures/tmp_figures/9.2_mortality_rates/', g_name, '.png', sep = '')
ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


# mortality rate among stand age 
g <- ggplot(df_sum, aes(x = AGE, y = mortality_rate, fill = PLOT_ID)) + # data
  geom_line(color = 'darkgray') +
  geom_point(color = 'black') +
  theme_minimal() + # graph theme
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15, hjust = 0.5),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15)) +
        legend.position = 'none') +
  labs(title =  'Mortality rate among the measurements of each site',
       x = 'Stand age (years)', y = 'Mortality rate (%)') + # labels       
  scale_fill_manual('Tree status', values = gray.colors(97, start = 0.1, end = 0.8)) +  # set color in grayscale
  facet_wrap(~INVENTORY_ID, scales = 'free') # one graph per inventory

g

# save graph
g_name <- 'mortality_per_measurements_and_site'
my_path <- paste('3_figures/tmp_figures/9.2_mortality_rates/', g_name, '.png', sep = '')
ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)


# mortality rate among SI
df_sum$SI_100_round <- round(df_sum$SI_100, 0)

ggplot(df_sum, aes(x = SI_100_round, y = mortality_rate, fill = INVENTORY_ID)) + # data
  geom_point() +
  theme_minimal() + # graph theme
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15, hjust = 0.5),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 15),
        legend.position = 'none') +
  labs(title =  'Mortality rate among Site Index',
       x = 'Site Index at age 100 (m)', y = 'Mortality rate (%)') + # labels       
  scale_fill_manual('Tree status', values = gray.colors(97, start = 0.1, end = 0.8))   # set color in grayscale
  #facet_wrap(~INVENTORY_ID, scales = 'free') # one graph per inventory

g

# save graph
#g_name <- 'mortality_per_measurements_and_site'
#my_path <- paste('3_figures/tmp_figures/9.2_mortality_rates/', g_name, '.png', sep = '')
#ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)

