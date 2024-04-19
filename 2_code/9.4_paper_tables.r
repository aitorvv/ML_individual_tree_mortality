#------------------------------------------------------------------------------------------#
####                            Explore df to get table data                            ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 22/02/2024                              #
#                              Last modification: 22/02/2024                               #
#------------------------------------------------------------------------------------------#


setwd('ML_individual_tree_mortality/')

library(dplyr)

#### Dataset size: big ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_big_time.csv')

# get climate data
clima <- df %>%
  group_by(site) %>%
  summarise(
    min_prec = min(prec),
    mean_prec = mean(prec),
    max_prec = max(prec),
    min_tmean = min(tmean),
    mean_tmean = mean(tmean),
    max_tmean = max(tmean)
  ) 

# get forest data
subplots <- df %>%
  group_by(site) %>%
  summarise(
    min_N = min(N),
    mean_N = mean(N),
    max_N = max(N),
    min_SDI = min(SDI),
    mean_SDI = mean(SDI),
    max_SDI = max(SDI),
    min_AGE = min(AGE),
    mean_AGE = mean(AGE),
    max_AGE = max(AGE),
    min_dg = min(Dg),
    mean_dg = mean(Dg),
    max_dg = max(Dg),
    min_year = min(year),
    max_year = max(year)
  )    
    
# export data
write.csv(clima, '1_data/1_original_df/8_tables/clima_table.csv', row.names = F)
write.csv(subplots, '1_data/1_original_df/8_tables/subplots_table.csv', row.names = F)
