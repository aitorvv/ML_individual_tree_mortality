#------------------------------------------------------------------------------------------#
####                                  Split datasets                                    ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 17/10/2023                              #
#                              Last modification: 04/11/2023                               #
#------------------------------------------------------------------------------------------#


setwd('ML_individual_tree_mortality/')

df_original <- read.csv('1_data/tmp_DEN/2_clima/df_complete_r33.csv')
df_original <- df_original[-1] # clean X variable created when writting a csv


#### Create different datasets based on the amount of data ####

# variables by sizes
df_small <- df_original[df_original$INVENTORY_ID == 'SON', ]
df_medium <- df_original[df_original$INVENTORY_ID %in% c('DEN', 'EUR', 'SON', 'VOH', 'WBU'), ]
df_big <- df_original

# when variables related with time are included, then the first measuremnt is not included on the analysis
# that means that I have to prepare additional data for that situation
tmp_df_small_time <-  tmp_df_medium_time <- tmp_df_big_time <- data.frame()

# for each plot
for(k in unique(df_small$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_small[df_small$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_small_time <- rbind(tmp_df_small_time, df_k)
}
  
# create new df
df_small_time <- tmp_df_small_time

# for each plot
for(k in unique(df_medium$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_medium[df_medium$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_medium_time <- rbind(tmp_df_medium_time, df_k)
}

# create new df
df_medium_time <- tmp_df_medium_time

# for each plot
for(k in unique(df_big$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_big[df_big$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_big_time <- rbind(tmp_df_big_time, df_k)
}

# create new df
df_big_time <- tmp_df_big_time

# delete initial plot records in a new df
df_original_not_initial <- df_original[df_original$i_year != 0, ]

# create small and medium size with the same amount of data, this time randomly selected
df_small_random <- sample(seq_len(nrow(df_original_not_initial)), size = nrow(df_small_time))
df_small_random <- df_original_not_initial[df_small_random, ]
df_medium_random <- sample(seq_len(nrow(df_original_not_initial)), size = nrow(df_medium_time))
df_medium_random <- df_original_not_initial[df_medium_random, ]

# delete tmp data
rm(k, start_year, tmp_df_big_time, tmp_df_medium_time, tmp_df_small_time, df_k, df_original_not_initial)


#### Create different datasets based on the thinning grade ####

# list of plots
A_grades <- list('5_1', '606_4', '612_7', '67_1', '68_1', '639_1', '622_3', '613_4', '603_2', '602_1', '607_3', '605_7', '605_8')
B_grades <- list('5_2', '67_2', '68_2', '639_4', '603_1', '607_8', '605_1', '605_2', '605_3', '605_4', '605_5', '605_6')
C_grades <- list('5_3', '67_3', '68_3', '602_2', '603_4', '606_6', '607_5', '607_8', '639_4')  
# the rest are D and E_grades

# define a new column to filter
df_original$t_grades <- ifelse(df_original$PLOT_ID %in% A_grades, 'A', 
                               ifelse(df_original$PLOT_ID %in% B_grades, 'B', 
                                      ifelse(df_original$PLOT_ID %in% C_grades, 'C',
                                             'D-E')))

df_original$thinning <- ifelse(df_original$t_grades == 'A', 'control', 
                               ifelse(df_original$t_grades %in% c('B', 'C'), 'below', 'above'))

# split datasets
df_control <- df_original[df_original$thinning == 'control', ]
df_below <- df_original[df_original$thinning == 'below', ]
df_above <- df_original[df_original$thinning == 'above', ]

# when variables related with time are included, then the first measuremnt is not included on the analysis
# that means that I have to prepare additional data for that situation
tmp_df_control_time <- tmp_df_below_time <- tmp_df_above_time <- data.frame()

# for each plot
for(k in unique(df_control$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_control[df_control$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_control_time <- rbind(tmp_df_control_time, df_k)
}

# create new df
df_control_time <- tmp_df_control_time

# for each plot
for(k in unique(df_below$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_below[df_below$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_below_time <- rbind(tmp_df_below_time, df_k)
}

# create new df
df_below_time <- tmp_df_below_time

# for each plot
for(k in unique(df_above$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_above[df_above$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_above_time <- rbind(tmp_df_above_time, df_k)
}

# create new df
df_above_time <- tmp_df_above_time

# delete tmp data
rm(k, start_year, tmp_df_above_time, tmp_df_below_time, tmp_df_control_time, df_k, A_grades, B_grades, C_grades)


#### Create different datasets based on the records length ####

# create empty df
df_new <- data.frame()

# for each plot...
for(plot in unique(df_original$PLOT_ID)){
  
  # get plot information
  plot <- df_original[df_original$PLOT_ID == plot, ]
  
  # get length of times inventoried
  years <- unique(plot$year)
  plot$n_records <- length(years)
  
  # append df with new values to the new df
  df_new <- rbind(df_new, plot)
}

# split new df into length ranges
df_3to5 <- df_new[df_new$n_records <= 5, ]
df_5to6 <- df_new[df_new$n_records > 5 & df_new$n_records <= 6, ]
df_6to7 <- df_new[df_new$n_records > 6 & df_new$n_records <= 7, ]
df_7to9 <- df_new[df_new$n_records > 7 & df_new$n_records <= 9, ]

# when variables related with time are included, then the first measuremnt is not included on the analysis
# that means that I have to prepare additional data for that situation
tmp_df_3to5_time <- tmp_df_5to6_time <- tmp_df_6to7_time <- tmp_df_7to9_time <- data.frame()

# for each plot
for(k in unique(df_3to5$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_3to5[df_3to5$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_3to5_time <- rbind(tmp_df_3to5_time, df_k)
}

# create new df
df_3to5_time <- tmp_df_3to5_time

# for each plot
for(k in unique(df_5to6$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_5to6[df_5to6$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_5to6_time <- rbind(tmp_df_5to6_time, df_k)
}

# create new df
df_5to6_time <- tmp_df_5to6_time

# for each plot
for(k in unique(df_6to7$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_6to7[df_6to7$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_6to7_time <- rbind(tmp_df_6to7_time, df_k)
}

# create new df
df_6to7_time <- tmp_df_6to7_time

# for each plot
for(k in unique(df_7to9$PLOT_ID)){
  
  # select plot data and initial year
  df_k <- df_7to9[df_7to9$PLOT_ID == k, ]
  start_year <- min(df_k$year)
  
  # delete initial year data and append the rest to a new df
  df_k <- df_k[df_k$year != start_year, ]
  tmp_df_7to9_time <- rbind(tmp_df_7to9_time, df_k)
}

# create new df
df_7to9_time <- tmp_df_7to9_time


#### Save results ####

# all
save.image('1_data/tmp_DEN/4_datasets/df_splitted.RData')

# size
write.csv(df_small, '1_data/tmp_DEN/4_datasets/df_small.csv', row.names = FALSE)
write.csv(df_medium, '1_data/tmp_DEN/4_datasets/df_medium.csv', row.names = FALSE)
write.csv(df_big, '1_data/tmp_DEN/4_datasets/df_big.csv', row.names = FALSE)

# size without initial year
write.csv(df_small_time, '1_data/tmp_DEN/4_datasets/df_small_time.csv', row.names = FALSE)
write.csv(df_small_random, '1_data/tmp_DEN/4_datasets/df_small_random_time.csv', row.names = FALSE)
write.csv(df_medium_time, '1_data/tmp_DEN/4_datasets/df_medium_time.csv', row.names = FALSE)
write.csv(df_medium_random, '1_data/tmp_DEN/4_datasets/df_medium_random_time.csv', row.names = FALSE)
write.csv(df_big_time, '1_data/tmp_DEN/4_datasets/df_big_time.csv', row.names = FALSE)

# thinning
write.csv(df_above, '1_data/tmp_DEN/4_datasets/df_above.csv', row.names = FALSE)
write.csv(df_below, '1_data/tmp_DEN/4_datasets/df_below.csv', row.names = FALSE)
write.csv(df_control, '1_data/tmp_DEN/4_datasets/df_control.csv', row.names = FALSE)

# thinning without initial year
write.csv(df_above_time, '1_data/tmp_DEN/4_datasets/df_above_time.csv', row.names = FALSE)
write.csv(df_below_time, '1_data/tmp_DEN/4_datasets/df_below_time.csv', row.names = FALSE)
write.csv(df_control_time, '1_data/tmp_DEN/4_datasets/df_control_time.csv', row.names = FALSE)

# records
write.csv(df_3to5, '1_data/tmp_DEN/4_datasets/df_3to5.csv', row.names = FALSE)
write.csv(df_5to6, '1_data/tmp_DEN/4_datasets/df_5to6.csv', row.names = FALSE)
write.csv(df_6to7, '1_data/tmp_DEN/4_datasets/df_6to7.csv', row.names = FALSE)
write.csv(df_7to9, '1_data/tmp_DEN/4_datasets/df_7to9.csv', row.names = FALSE)

# records without initial year
write.csv(df_3to5_time, '1_data/tmp_DEN/4_datasets/df_3to5_time.csv', row.names = FALSE)
write.csv(df_5to6_time, '1_data/tmp_DEN/4_datasets/df_5to6_time.csv', row.names = FALSE)
write.csv(df_6to7_time, '1_data/tmp_DEN/4_datasets/df_6to7_time.csv', row.names = FALSE)
write.csv(df_7to9_time, '1_data/tmp_DEN/4_datasets/df_7to9_time.csv', row.names = FALSE)
