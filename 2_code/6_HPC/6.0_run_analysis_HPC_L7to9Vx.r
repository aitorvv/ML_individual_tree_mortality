#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                               Binary classification                                
#                                                                                          #
#                            Aitor Vázquez Veloso, 12/05/2023                              #
#                              Last modification: 27/10/2023                               #
#------------------------------------------------------------------------------------------#
 
# On that document different methodologies were used to perform binary classification analysis.
# All the analysis were developed using the "caret" library, while Artificial Neural Networks
# were developed on Python (externally), using Tensorflow and Keras.

#### Selection section ####

#df_size = 'big'  # small, medium, big
var_size = 'extreme'  # easy, medium, hard, extreme
df_len = '7to9'  # 3to5, 5to6, 6to7, 7to9


#### Basic steops ####

# libraries
library(tidyverse)
#library(naivebayes) # NV model
#library(ggpubr)
#library(mvnormtest) # mshapiro_test()
#library(corrplot) # correlation features plot
library(caret) # easy ML workflow
library(ROCR) # metrics from models performance
#library(mlr3measures) # bacc metric
library(irr) # kappa coefficient
#library(leaps) # stepwise
#library(MASS) # AIC to choose the best model
#library(class) # KNN model
#library(randomForest) # RF
#library(e1071) # SVM
#library(neuralnet) # ANN
library(RJSONIO) # read results from Python neural networks
library(tictoc) # check time for training each model
# library(parallel) # paralelization
# library(doParallel) # paralelization

# working directory
setwd('/home/aitorvazquez/PhD/Vitality')

# original data
if(var_size %in% c('easy', 'medium', 'hard', 'extreme')){  # in that cases, the first record of each plot is not included
  df_original <- read.csv(paste('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/df_', df_len, '_time.csv', sep = ''))
} else {
  df_original <- read.csv(paste('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/df_', df_len, '.csv', sep = ''))
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


#### Perform LR analysis ####

# call functions
source('3_final/scripts/6.1_LR_analysis_HPC.r')

# perform analysis
LR_results <- LR_analysis(df_study = df_study, my_combis = my_combis, my_timer = my_timer)

# unpack results
lr_model <- LR_results[[1]]
lr_accuracy <- LR_results[[2]]
lr_accuracy_dead <- LR_results[[3]]
lr_accuracy_alive <- LR_results[[4]]
lr_roc <- LR_results[[5]]
lr_auc <- LR_results[[6]]
lr_aucpr <- LR_results[[7]]
lr_mcc <- LR_results[[8]]
lr_kappa <- LR_results[[9]]
lr_threshold <- LR_results[[10]]
my_timer <- LR_results[[11]]
rm(LR_results, LR_analysis, LR_analysis_parallel)
print('LR analysis have finished successfully.')


#### Perform DT analysis ####

# call functions
source('3_final/scripts/6.2_DT_analysis_HPC.r')

# perform analysis
DT_results <- DT_analysis(df_study = df_study, my_combis = my_combis, my_timer = my_timer)

# unpack results
dt_model <- DT_results[[1]]
dt_accuracy <- DT_results[[2]]
dt_accuracy_dead <- DT_results[[3]]
dt_accuracy_alive <- DT_results[[4]]
dt_roc <- DT_results[[5]]
dt_auc <- DT_results[[6]]
dt_aucpr <- DT_results[[7]]
dt_mcc <- DT_results[[8]]
dt_kappa <- DT_results[[9]]
dt_threshold <- DT_results[[10]]
my_timer <- DT_results[[11]]
rm(DT_results, DT_analysis, DT_analysis_parallel)
print('DT analysis have finished successfully.')


#### Perform RF analysis ####

# call functions
source('3_final/scripts/6.3_RF_analysis_HPC.r')

# perform analysis
RF_results <- RF_analysis(df_study = df_study, my_combis = my_combis, my_timer = my_timer)

# unpack results
rf_model <- RF_results[[1]]
rf_accuracy <- RF_results[[2]]
rf_accuracy_dead <- RF_results[[3]]
rf_accuracy_alive <- RF_results[[4]]
rf_roc <- RF_results[[5]]
rf_auc <- RF_results[[6]]
rf_aucpr <- RF_results[[7]]
rf_mcc <- RF_results[[8]]
rf_kappa <- RF_results[[9]]
rf_threshold <- RF_results[[10]]
my_timer <- RF_results[[11]]
rm(RF_results, RF_analysis, RF_analysis_parallel)
print('RF analysis have finished successfully.')


#### Perform NB analysis ####

# call functions
source('3_final/scripts/6.4_NB_analysis_HPC.r')

# perform analysis
NB_results <- NB_analysis(df_study = df_study, my_combis = my_combis, my_timer = my_timer)

# unpack results
nb_model <- NB_results[[1]]
nb_accuracy <- NB_results[[2]]
nb_accuracy_dead <- NB_results[[3]]
nb_accuracy_alive <- NB_results[[4]]
nb_roc <- NB_results[[5]]
nb_auc <- NB_results[[6]]
nb_aucpr <- NB_results[[7]]
nb_mcc <- NB_results[[8]]
nb_kappa <- NB_results[[9]]
nb_threshold <- NB_results[[10]]
my_timer <- NB_results[[11]]
rm(NB_results, NB_analysis, NB_analysis_parallel)
print('NB analysis have finished successfully.')


#### Perform KNN analysis ####

# call functions
source('3_final/scripts/6.5_KNN_analysis_HPC.r')

# perform analysis
KNN_results <- KNN_analysis(df_study = df_study, my_combis = my_combis, my_timer = my_timer)

# unpack results
knn_model <- KNN_results[[1]]
knn_accuracy <- KNN_results[[2]]
knn_accuracy_dead <- KNN_results[[3]]
knn_accuracy_alive <- KNN_results[[4]]
knn_roc <- KNN_results[[5]]
knn_auc <- KNN_results[[6]]
knn_aucpr <- KNN_results[[7]]
knn_mcc <- KNN_results[[8]]
knn_kappa <- KNN_results[[9]]
knn_threshold <- KNN_results[[10]]
my_timer <- KNN_results[[11]]
rm(KNN_results, KNN_analysis, KNN_analysis_parallel)
print('KNN analysis have finished successfully.')


#### Perform SVM analysis ####

# call functions
source('3_final/scripts/6.6_SVM_analysis_HPC.r')

# perform analysis
SVM_results <- SVM_analysis(df_study = df_study, my_combis = my_combis, my_timer = my_timer)

# unpack results
svm_model <- SVM_results[[1]]
svm_accuracy <- SVM_results[[2]]
svm_accuracy_dead <- SVM_results[[3]]
svm_accuracy_alive <- SVM_results[[4]]
svm_roc <- SVM_results[[5]]
svm_auc <- SVM_results[[6]]
svm_aucpr <- SVM_results[[7]]
svm_mcc <- SVM_results[[8]]
svm_kappa <- SVM_results[[9]]
# svm_threshold <- SVM_results[[10]]  # not available
my_timer <- SVM_results[[10]]
rm(SVM_results, SVM_analysis, SVM_analysis_parallel)
print('SVM analysis have finished successfully.')


#### Save results ####

# save all my objects 
save(lr_model, dt_model, rf_model, nb_model, svm_model, knn_model, file = 
     paste('3_final/data/5_analysis/L', df_len, '_V', var_size, '/models.RData', sep = ''))
print('Full workspace saved')

# delete models
rm(lr_model, dt_model, rf_model, nb_model, svm_model, knn_model, normalize)

# save results without models
save.image(file =  paste('3_final/data/5_analysis/L', df_len, '_V', var_size, '/metrics.RData', sep = ''))
print('Light workspace saved')
