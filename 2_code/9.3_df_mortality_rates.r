#------------------------------------------------------------------------------------------#
####                          Get mortality rates for each df                           ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 08/11/2023                              #
#                              Last modification: 08/11/2023                               #
#------------------------------------------------------------------------------------------#


setwd('ML_individual_tree_mortality/')


#### Dataset size: small ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_small_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Dataset size: small & random ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_small_random_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Dataset size: medium ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_medium_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Dataset size: medium & random ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_medium_random_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Dataset size: big ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_big_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Thinning from above ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_above_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Thinning from below ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_below_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Unthined ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_control_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Records length: 3 to 5 ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_3to5_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Records length: 6 ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_5to6_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Records length: 7 ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_6to7_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)


#### Records length: 7 to 9 ####

# read data
df <- read.csv('1_data/tmp_DEN/4_datasets/df_7to9_time.csv')

# calculate nº dead trees and proportion
df$mortality <- ifelse(df$dead == 'no', 0, 1)
df$mortality <- as.numeric(df$mortality)
sum(df$mortality)
sum(df$mortality)/nrow(df)

