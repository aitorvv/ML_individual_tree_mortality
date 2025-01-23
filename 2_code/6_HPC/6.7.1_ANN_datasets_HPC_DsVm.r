#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                            ANN binary classification                                
#                                                                                          #
#                            Aitor Vázquez Veloso, 12/05/2023                              #
#                              Last modification: 18/10/2023                               #
#------------------------------------------------------------------------------------------#


####  ANN - combined with Python-TensorFlow ####

# Neuralnet was not performing well, so I decided to preserve the code in R, extract 
# input data for neural networks and perform them on Python by using TensorFlow.
# After that, the predictions for each combination are charged on R again to continue
# with the analysis.
# That code will be divided in two steps, one before and other after Python intervention.

#### Selection section ####

df_size = 'small'  # small, medium, big
var_size = 'medium'  # easy, medium, hard, extreme

#### Basic steops ####

# libraries
library(tidyverse)
library(caret) # easy ML workflow
library(ROCR) # metrics from models performance
library(irr) # kappa coefficient
library(RJSONIO) # read results from Python neural networks
library(tictoc) # check time for training each model

# working directory
setwd('/home/aitorvazquez/PhD/Vitality')

# original data
if(var_size %in% c('easy', 'medium', 'hard', 'extreme')){  # in that cases, the first record of each plot is not included
  df_original <- read.csv(paste('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/df_', df_size, '_time.csv', sep = ''))
} else {
  df_original <- read.csv(paste('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/df_', df_size, '.csv', sep = ''))
}

# timer
my_timer <- tibble()

# Normalization function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}


#### Dataset filters ####

# keep a copy of the original data
df_study <- df_original

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### Variable combinations ####

# load variable combinations
my_combis <- readRDS(paste('3_final/data/4_datasets/combis_', var_size, '.RDS', sep = ''))

print('#-----------------------#')
print('Data preprocessing ready!')
print('#-----------------------#')


##### 1. Generate data for Python neural network ####

# activate timer
tic("ANN_1")

# model number
n_model = 0

# lists: models and accuracy
ann_model <- list()
ann_accuracy <- list()
ann_accuracy_dead <- list()
ann_accuracy_alive <- list()
ann_roc <- list()
ann_auc <- list()
ann_aucpr <- list()
ann_mcc <- list()
ann_kappa <- list()
ann_threshold <- list()

for (k in my_combis){
  
  # model name
  n_model = n_model + 1
  name <- paste('ann_model_', n_model, sep = '')
  
  # get my variables combination
  df <- df_study[, which((names(df_study) %in% k) == TRUE)]
  
  # normalize data
  dead <- df$dead
  df <- normalize(df[2:length(df)])
  df$dead <- dead
  
  # split data
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  #labels <- ifelse(test$dead == 'yes', 1, 0)
  #test <- dplyr::select(test, -dead)
  train$dead <- ifelse(train$dead == 'yes', 1, 0)
  test$dead <- ifelse(test$dead == 'yes', 1, 0)
  
  # export data
  write.csv(train, paste('3_final/data/5_analysis/ann/input/D', df_size, '_V', var_size, '/train/', name, '.csv', sep = ''), row.names = FALSE)
  write.csv(test, paste('3_final/data/5_analysis/ann/input/D', df_size, '_V', var_size, '/test/', name, '.csv', sep = ''), row.names = FALSE)
}

# save time data
my_timer <- rbind(my_timer, toc())

# save my_timer
write.csv(my_timer, paste('3_final/data/5_analysis/ann/timer/D', df_size, '_V', var_size, '/ANN_1_timer.csv', sep = ''), row.names = FALSE)
