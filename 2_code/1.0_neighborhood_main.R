#------------------------------------------------------------------------------------------#
####                                 Trees neighborhood                                 ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 31/05/2023                              #
#                              Last modification: 13/06/2023                               #
#------------------------------------------------------------------------------------------#

# That code was originally developed to a test dataset. Now it is adapted to the new one

#### Initial setup ####

library(tidyverse)
library(sf)
library(beepr)

setwd('ML_individual_tree_mortality/')

df <- read.csv('1_data/tmp_DEN/0_initial_df_clean/initial_df_clean.csv', header = TRUE, sep = ',')

# Nr is the tree code
# Artcode is species: 10 = Picea abies, 30 = Pinus sylvestris, 70 = Pseudotsuga menziessii
# removed doesn't make difference between natural or thinning mortality

# get functions
source(file = '2_code/1.1_neighborhood_functions.R')
source(file = '2_code/SI/bonitiere_v3.R')


#### Features setup ####

# set radius for neigborhood
radius_height <- 0.33 # 0.25, 0.33, 0.42, 0.50
# dealing with initial data
trees <- manage_data(df = df, radius_height = radius_height)


#### Neighborhood calculation ####

# variables to fill with data
neighborhood_stats <- as_tibble(as.data.frame(matrix(nrow = 0, ncol = 0)))
subplot_stats <- as_tibble(as.data.frame(matrix(nrow = 0, ncol = 0)))

# calculate neighborhood and subplots for each tree
for (i in unique(trees$PLOT_ID_time)){
    
  # select plot
  plot <- trees[trees$PLOT_ID_time == i, ]
  # distances between trees
  dist_plot <- get_neighbors(plot = plot)
  dist_plot <- dist_plot[dist_plot$distance != 0, ]
  # neighborhood and subplot stats
  stats <- get_subplots(plot = plot, dist_plot = dist_plot)
  neighborhood_stats_i <- stats$neighborhood
  subplot_stats_i <- stats$subplot
  # extra subplot calculations
  subplot_stats_i <- calc_subplot(subplot_stats = subplot_stats_i,
                                  trees_in_subplot = neighborhood_stats_i)
  # store data
  neighborhood_stats <- rbind(neighborhood_stats, neighborhood_stats_i)
  subplot_stats <- rbind(subplot_stats, subplot_stats_i)
}


#### SI calculation ####

# df for save SI values
SI <- tibble()

# calculate SI per plot
for (plot in unique(subplot_stats$PLOT_ID)){

  # extract data
  plot <- subplot_stats[subplot_stats$PLOT_ID == plot, ]
  
  # calculate SI just if AGE value is available
  if(!is.na(unique(plot$AGE))){
    plot$SI_100 <- mapply(bonitiere.art, 
                            art = 10, # species
                            alter = plot$AGE, # age 
                            hoehe = plot$Ho, # reference data
                            MoreArgs = list(bezug.alter = 100)) # age to calculate SI
  } else {
    plot$SI_100 <- ''
  }
  
  # save results
  SI <- rbind(SI, plot)
}

# just change name of df
subplot_stats <- SI


#### Tree size relationships ####

trees <- size_rels(trees=trees, 
                   subplot_stats=subplot_stats) 


# remove temp information
rm(dist_plot, neighborhood_stats_i, plot, stats, subplot_stats_i, i, 
   AlturaDominante, calc_subplot, DiametroDominante, get_neighbors, get_subplots,
   manage_data, size_rels, round_up, SI, boni.faecher.bu, boni.faecher.dg, boni.faecher.ei,
   boni.faecher.er, boni.faecher.fi, boni.faecher.fi.hg, boni.faecher.ki, boni.faecher.la,
   boni.faecher.ta, bonitaet, bonitiere.art, chapman)


#### Manage data type and prepare them for classification ####

## Clean and adapt data type

# check missing values
sapply(subplot_stats, function(x) sum(is.na(x)))
sapply(trees, function(x) sum(is.na(x)))

# check unique values
sapply(subplot_stats, function(x) length(unique(x)))
sapply(trees, function(x) length(unique(x)))

# check variables type
str(subplot_stats)  # all numeric except IDs
str(trees)

# convert some to factor
trees$removed <- as_factor(trees$removed)
trees$dead <- as_factor(trees$dead)
trees$thinned <- as.factor(trees$thinned)
trees$species <- as_factor(trees$species)

# check how factors are structured
contrasts(trees$removed)
contrasts(trees$dead)
contrasts(trees$thinned)


#### Checkpoint ####

# save data
write.csv(trees, paste('1_data/tmp_DEN/1_neighborhood/trees_r', substring(radius_height, 3, 4), '.csv', sep = ''), fileEncoding = 'UTF-8')
write.csv(subplot_stats, paste('1_data/tmp_DEN/1_neighborhood/subplot_stats_r', substring(radius_height, 3, 4), '.csv', sep = ''), fileEncoding = 'UTF-8')
write.csv(neighborhood_stats, paste('1_data/tmp_DEN/1_neighborhood/neighborhood_stats_r', substring(radius_height, 3, 4), '.csv', sep = ''), fileEncoding = 'UTF-8')

# make some sound to indicate the finish
beep(sound = 8, expr = NULL)
