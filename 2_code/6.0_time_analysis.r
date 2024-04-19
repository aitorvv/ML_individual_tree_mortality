#------------------------------------------------------------------------------------------#
####                             Time spent into training                                
#                                                                                          #
#                            Aitor Vázquez Veloso, 12/05/2023                              #
#                              Last modification: 18/01/2024                               #
#------------------------------------------------------------------------------------------#


#### Basic steops ####

# libraries
library(tidyverse)
library(caret) # easy ML workflow
library(ROCR) # metrics from models performance
library(irr) # kappa coefficient
library(tictoc) # check time for training each model

# working directory
setwd('ML_individual_tree_mortality/')

# read combis data
my_combis_easy <- readRDS('1_data/tmp_DEN/4_datasets/combis_easy.RDS')
my_combis_medium <- readRDS('1_data/tmp_DEN/4_datasets/combis_medium.RDS')
my_combis_hard <- readRDS('1_data/tmp_DEN/4_datasets/combis_hard.RDS')
my_combis_extreme <- readRDS('1_data/tmp_DEN/4_datasets/combis_extreme.RDS')

# extract the different number of combinations
list_combis <- c(my_combis_easy, my_combis_medium, my_combis_hard, my_combis_extreme)
combis_len <- c()
combi_selection <- c()
for(k in 1:length(list_combis)){
  
  len <- length(list_combis[[k]])
  ifelse(len %in% combis_len, next, 
         combi_selection <- c(combi_selection, k))
  combis_len <- c(combis_len, len)
}

# extract the combinations
final_combis <- list_combis[combi_selection]

# clean environment
rm(list = ls()[! ls() %in% c('final_combis')])

# read df size data
df_small <- read.csv('1_data/tmp_DEN/4_datasets/df_small_random_time.csv')
df_medium <- read.csv('1_data/tmp_DEN/4_datasets/df_medium_random_time.csv')
df_big <- read.csv('1_data/tmp_DEN/4_datasets/df_big_time.csv')
df_list <- list(df_small, df_medium, df_big)

# start timer
my_timer <- tibble()

# Normalization function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# call functions
source('2_code/5.1_LR_analysis.r')
source('2_code/5.2_DT_analysis.r')
source('2_code/5.3_RF_analysis.r')
source('2_code/5.4_NB_analysis.r')
source('2_code/5.5_KNN_analysis.r')
source('2_code/5.6_SVM_analysis.r')

#### Start analysis ####

# counters
c <- d <- 0

# for each variable combination
for(combi in final_combis){
  
  # convert to list
  combi <- list(combi)
  
  # update counter
  c <- c + 1  
  
  # for each dataset size
  for(df in df_list){
    
    # update counter
    d <- d + 1
    
    # reorder variables to have dead always as the first one (important later)
    df <- dplyr::select(df, dead, everything())
    
    # divide data: 80% train and 20% test
    smp_size <- floor(0.8 * nrow(df))
    
    # set the seed to make your partition reproducible
    set.seed(33)
    
    # perform analysis, save timer and remove the rest: LR
    my_timer <- LR_analysis_timer(df_study = df, my_combis = combi, my_timer = my_timer)
   
    # perform analysis, save timer and remove the rest: DT
    my_timer <- DT_analysis_timer(df_study = df, my_combis = combi, my_timer = my_timer)
    
    # perform analysis, save timer and remove the rest: RF
    my_timer <- RF_analysis_timer(df_study = df, my_combis = combi, my_timer = my_timer)
    
    # perform analysis, save timer and remove the rest: NB
    my_timer <- NB_analysis_timer(df_study = df, my_combis = combi, my_timer = my_timer)
    
    # perform analysis, save timer and remove the rest: KNN
    my_timer <- KNN_analysis_timer(df_study = df, my_combis = combi, my_timer = my_timer)
    
    # perform analysis, save timer and remove the rest: SVM
    my_timer <- SVM_analysis_timer(df_study = df, my_combis = combi, my_timer = my_timer)
    
    # print message
    print(paste('Finished analysis for combination', c, 'and dataframe', d, sep = ' '))
  }
}

# remove all except timer and save it
rm(list = ls()[! ls() %in% c('my_timer')])
save.image(file = '1_data/tmp_DEN/6_final_results/timer/timer_comparison.RData')
