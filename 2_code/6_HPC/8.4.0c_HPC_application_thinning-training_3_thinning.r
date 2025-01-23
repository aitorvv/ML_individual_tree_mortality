#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                      Application on thinning - HPC predictions                     ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 06/11/2023                              #
#                              Last modification: 22/01/2025                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
library(caret)
library(ROCR)

setwd('/home/aitorvazquez/PhD/Vitality/3_final/data/7_applications/')


#### Load general information ####

# load the variables groups from the previous code
load('/home/aitorvazquez/PhD/Vitality/3_final/data/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, normalize, time_graph)


#### 3 thinning models: model selection ####

# already selected models
models_list <- readRDS('/home/aitorvazquez/PhD/Vitality/3_final/data/7_applications/3_thinning_models.rds')
my_models <- readRDS('/home/aitorvazquez/PhD/Vitality/3_final/data/7_applications/3_thinning_models_summary.rds')

# read combis 
my_combis <- readRDS('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/combis_hard.RDS')

print('Loaded information for 3 thinning models.')


#### 3 thinning models: from above data selection ####

# Normalization function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# create final df with metrics
case_metrics <- data.frame()

# set values of the case study
model_used <- 'Dbig_Vhard'
data_used <- 'Tabove_Vhard'

# read data
df_study <- read.csv('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/df_above_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### 3 thinning models: from above predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
  print(paste('3 thinning models + above df: ', k, sep = ''))
  
  # select classifier
  classifier_name <- names_methods[k]
  
  # select model
  model <- models_list[k]
  
  # select case model
  case_model <- my_models[my_models$names_methods %in% classifier_name, ] 
  case_model <- as.numeric(case_model$best_model)
  
  # get my variables combination
  df <- df_study[, which((names(df_study) %in% my_combis[[case_model]]) == TRUE)]
  
  # normalize data
  dead_col <- df$dead
  df <- normalize(df[2:length(df)])
  df$dead <- dead_col
  
  # split data
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  train <- df[train_ind, ]
  train$dead <- ifelse(train$dead == 'yes', 1, 0)
  
  test <- df[-train_ind, ]
  labels <- ifelse(test$dead == 'yes', 1, 0)
  test <- dplyr::select(test, -dead)
  
  # predict, threshold and binary classification
  if(classifier_name %in% 'SVM'){
    
    # binary classify predictions
    pred = predict(model[[1]], newdata = test, type = 'raw')  
    
    # binary classify predictions
    pred <- ifelse(as.numeric(pred) == 2, 1, 0)
    
  } else {
    
    # make a prediction 
    pred <- predict(model[[1]], newdata = test, type = 'prob')
    
    # depending on the classifier
    if(classifier_name %in% c('RF', 'NB', 'KNN')){
      
      # choose the best threshold based on MCC
      t_mcc <- 0
      threshold <- 0
      for(t in seq(0.001, 0.999, by = 0.001)){
        t_pred <- ifelse(pred[, 'yes'] > t, 1, 0)
        t_pred_vs_real <- ROCR::prediction(t_pred, labels)
        t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
        if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
           t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
          t_mcc <- t_mcc_tmp@y.values[[1]][2]
          threshold <- t
        } 
      }
      
      # binary classify predictions
      pred <- ifelse(pred[, 'yes'] > threshold, 1, 0)
      
    } else {
      
      # choose the best threshold based on MCC
      t_mcc <- 0
      threshold <- 0
      for(t in seq(0.001, 0.999, by = 0.001)){
        t_pred <- ifelse(pred[, '1'] > t, 1, 0)
        t_pred_vs_real <- ROCR::prediction(t_pred, labels)
        t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
        if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
           t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
          t_mcc <- t_mcc_tmp@y.values[[1]][2]
          threshold <- t
        } 
      }
      
      # binary classify predictions    
      pred <- ifelse(pred[, '1'] > threshold, 1, 0)
      
    }
  }
  
  # ROC curve, plotting TPR (True Positive Rate) against the FPR (False Positive Rate)
  pred_vs_real <- ROCR::prediction(pred, labels)
  
  # MCC - Matthews correlation coefficient or phi coefficient
  mcc <- ROCR::performance(pred_vs_real, measure = "phi")
  mcc <- mcc@y.values[[1]]
  
  # save metric
  tmp_case_metrics <- data.frame(classifier = classifier_name, model_used = model_used, data_used = data_used, metric = 'mcc', pred_acc = mcc[2])
  case_metrics <- rbind(case_metrics, tmp_case_metrics)
  
}

print('Predictions with data from above done!')


#### 3 thinning models: from below data selection ####

# set values of the case study
data_used <- 'Tbelow_Vhard'

# read data
df_study <- read.csv('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/df_below_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### 3 thinning models: from below predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
  print(paste('3 thinning models + below df: ', k, sep = ''))
  
  # select classifier
  classifier_name <- names_methods[k]
  
  # select model
  model <- models_list[k]
  
  # select case model
  case_model <- my_models[my_models$names_methods %in% classifier_name, ] 
  case_model <- as.numeric(case_model$best_model)
  
  # get my variables combination
  df <- df_study[, which((names(df_study) %in% my_combis[[case_model]]) == TRUE)]
  
  # normalize data
  dead_col <- df$dead
  df <- normalize(df[2:length(df)])
  df$dead <- dead_col
  
  # split data
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  train <- df[train_ind, ]
  train$dead <- ifelse(train$dead == 'yes', 1, 0)
  
  test <- df[-train_ind, ]
  labels <- ifelse(test$dead == 'yes', 1, 0)
  test <- dplyr::select(test, -dead)
  
  # predict, threshold and binary classification
  if(classifier_name %in% 'SVM'){
    
    # binary classify predictions
    pred = predict(model[[1]], newdata = test, type = 'raw')  
    
    # binary classify predictions
    pred <- ifelse(as.numeric(pred) == 2, 1, 0)
    
  } else {
    
    # make a prediction 
    pred <- predict(model[[1]], newdata = test, type = 'prob')
    
    # depending on the classifier
    if(classifier_name %in% c('RF', 'NB', 'KNN')){
      
      # choose the best threshold based on MCC
      t_mcc <- 0
      threshold <- 0
      for(t in seq(0.001, 0.999, by = 0.001)){
        t_pred <- ifelse(pred[, 'yes'] > t, 1, 0)
        t_pred_vs_real <- ROCR::prediction(t_pred, labels)
        t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
        if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
           t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
          t_mcc <- t_mcc_tmp@y.values[[1]][2]
          threshold <- t
        } 
      }
      
      # binary classify predictions
      pred <- ifelse(pred[, 'yes'] > threshold, 1, 0)
      
    } else {
      
      # choose the best threshold based on MCC
      t_mcc <- 0
      threshold <- 0
      for(t in seq(0.001, 0.999, by = 0.001)){
        t_pred <- ifelse(pred[, '1'] > t, 1, 0)
        t_pred_vs_real <- ROCR::prediction(t_pred, labels)
        t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
        if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
           t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
          t_mcc <- t_mcc_tmp@y.values[[1]][2]
          threshold <- t
        } 
      }
      
      # binary classify predictions    
      pred <- ifelse(pred[, '1'] > threshold, 1, 0)
      
    }
  }
  
  # ROC curve, plotting TPR (True Positive Rate) against the FPR (False Positive Rate)
  pred_vs_real <- ROCR::prediction(pred, labels)
  
  # MCC - Matthews correlation coefficient or phi coefficient
  mcc <- ROCR::performance(pred_vs_real, measure = "phi")
  mcc <- mcc@y.values[[1]]
  
  # save metric
  tmp_case_metrics <- data.frame(classifier = classifier_name, model_used = model_used, data_used = data_used, metric = 'mcc', pred_acc = mcc[2])
  case_metrics <- rbind(case_metrics, tmp_case_metrics)
  
}

print('Predictions with data from below done!')



#### 3 thinning models: control data selection ####

# set values of the case study
data_used <- 'Tcontrol_Vhard'

# read data
df_study <- read.csv('/home/aitorvazquez/PhD/Vitality/3_final/data/4_datasets/df_control_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### 3 thinning models: control predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
  print(paste('3 thinning models + control df: ', k, sep = ''))
  
  # select classifier
  classifier_name <- names_methods[k]
  
  # select model
  model <- models_list[k]
  
  # select case model
  case_model <- my_models[my_models$names_methods %in% classifier_name, ] 
  case_model <- as.numeric(case_model$best_model)
  
  # get my variables combination
  df <- df_study[, which((names(df_study) %in% my_combis[[case_model]]) == TRUE)]
  
  # normalize data
  dead_col <- df$dead
  df <- normalize(df[2:length(df)])
  df$dead <- dead_col
  
  # split data
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  train <- df[train_ind, ]
  train$dead <- ifelse(train$dead == 'yes', 1, 0)
  
  test <- df[-train_ind, ]
  labels <- ifelse(test$dead == 'yes', 1, 0)
  test <- dplyr::select(test, -dead)
  
  # predict, threshold and binary classification
  if(classifier_name %in% 'SVM'){
    
    # binary classify predictions
    pred = predict(model[[1]], newdata = test, type = 'raw')  
    
    # binary classify predictions
    pred <- ifelse(as.numeric(pred) == 2, 1, 0)
    
  } else {
    
    # make a prediction 
    pred <- predict(model[[1]], newdata = test, type = 'prob')
    
    # depending on the classifier
    if(classifier_name %in% c('RF', 'NB', 'KNN')){
      
      # choose the best threshold based on MCC
      t_mcc <- 0
      threshold <- 0
      for(t in seq(0.001, 0.999, by = 0.001)){
        t_pred <- ifelse(pred[, 'yes'] > t, 1, 0)
        t_pred_vs_real <- ROCR::prediction(t_pred, labels)
        t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
        if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
           t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
          t_mcc <- t_mcc_tmp@y.values[[1]][2]
          threshold <- t
        } 
      }
      
      # binary classify predictions
      pred <- ifelse(pred[, 'yes'] > threshold, 1, 0)
      
    } else {
      
      # choose the best threshold based on MCC
      t_mcc <- 0
      threshold <- 0
      for(t in seq(0.001, 0.999, by = 0.001)){
        t_pred <- ifelse(pred[, '1'] > t, 1, 0)
        t_pred_vs_real <- ROCR::prediction(t_pred, labels)
        t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
        if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
           t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
          t_mcc <- t_mcc_tmp@y.values[[1]][2]
          threshold <- t
        } 
      }
      
      # binary classify predictions    
      pred <- ifelse(pred[, '1'] > threshold, 1, 0)
      
    }
  }
  
  # ROC curve, plotting TPR (True Positive Rate) against the FPR (False Positive Rate)
  pred_vs_real <- ROCR::prediction(pred, labels)
  
  # MCC - Matthews correlation coefficient or phi coefficient
  mcc <- ROCR::performance(pred_vs_real, measure = "phi")
  mcc <- mcc@y.values[[1]]
  
  # save metric
  tmp_case_metrics <- data.frame(classifier = classifier_name, model_used = model_used, data_used = data_used, metric = 'mcc', pred_acc = mcc[2])
  case_metrics <- rbind(case_metrics, tmp_case_metrics)
  
}

print('Predictions with control data done!')


#### Save results ####

# remove everything except metrics
rm(list=setdiff(ls(), "case_metrics"))

# save metrics
save.image('/home/aitorvazquez/PhD/Vitality/3_final/data/7_applications/case_metrics_3_thinning_models.RData')

# remove all
rm(list=ls())
