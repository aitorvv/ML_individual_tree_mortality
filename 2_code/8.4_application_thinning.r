#------------------------------------------------------------------------------------------#
####                      Application on thinning - predictions                         ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 06/11/2023                              #
#                              Last modification: 06/11/2023                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
library(caret)
library(ROCR)

setwd('ML_individual_tree_mortality/')


#### Load general information ####

# load the variables groups from the previous code
load('1_data/1_original_df/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, normalize, time_graph)


#### Unthinned model and thinned data: model selection ####

# # after inspect previous results, I choose the models with the "hard" type of variables
# # I filter the best model to know which sould I use
# my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tcontrol_Vhard', ]
# my_models <- my_models[my_models$Metrics %in% 'mcc', ]
# 
# # I pick the models already trained
# load('1_data/3_final/5_analysis/Tcontrol_Vhard/models.RData')
# 
# # select the models needed
# lr_m <- lr_model$lr_model_27
# dt_m <- dt_model$dt_model_44
# rf_m <- rf_model$rf_model_7
# nb_m <- nb_model$nb_model_7
# knn_m <- knn_model$knn_model_26
# svm_m <- svm_model$svm_model_34
# models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)
# 
# # and delete the rest to save space
# rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
#    lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# already selected models
models_list <- readRDS('1_data/1_original_df/7_applications/control_models.rds')
my_models <- readRDS('1_data/1_original_df/7_applications/control_models_summary.rds')

# read combis 
my_combis <- readRDS('1_data/tmp_DEN/4_datasets/combis_hard.RDS')


#### Unthinned model and thinned data: from above data selection ####

# Normalization function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# create final df with metrics
case_metrics <- data.frame()

# set values of the case study
model_used <- 'Tcontrol_Vhard'
data_used <- 'Tabove_Vhard'

# read data
df_study <- read.csv('1_data/tmp_DEN/4_datasets/df_above_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### Unthinned model and thinned data: from above predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
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


#### Unthinned model and thinned data: from below data selection ####

# set values of the case study
data_used <- 'Tbelow_Vhard'

# read data
df_study <- read.csv('1_data/tmp_DEN/4_datasets/df_below_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### Unthinned model and thinned data: from below predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
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


#### Save results ####

# remove everything except metrics
rm(list=setdiff(ls(), "case_metrics"))

# save metrics
save.image('1_data/1_original_df/7_applications/case_metrics_control.RData')

# remove all
rm(list=ls())


#### +-+-+- Break to change the case +-+-+- ####


#### Load general information ####

# load the variables groups from the previous code
load('1_data/1_original_df/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, normalize, time_graph)


#### Tabove model and other data: model selection ####

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
# my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tabove_Vhard', ]
# my_models <- my_models[my_models$Metrics %in% 'mcc', ]
# 
# # I pick the models already trained
# load('1_data/3_final/5_analysis/Tabove_Vhard/models.RData')
# 
# # select the models needed
# lr_m <- lr_model$lr_model_79
# dt_m <- dt_model$dt_model_18
# rf_m <- rf_model$rf_model_70
# nb_m <- nb_model$nb_model_75
# knn_m <- knn_model$knn_model_3
# svm_m <- svm_model$svm_model_81
# models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)
# 
# # and delete the rest to save space
# rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
#    lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# already selected models
models_list <- readRDS('1_data/1_original_df/7_applications/above_models.rds')
my_models <- readRDS('1_data/1_original_df/7_applications/above_models_summary.rds')

# read combis 
my_combis <- readRDS('1_data/tmp_DEN/4_datasets/combis_hard.RDS')


#### Tabove model and other data: control data selection ####

# Normalization function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# create final df with metrics
case_metrics <- data.frame()

# set values of the case study
model_used <- 'Tabove_Vhard'
data_used <- 'Tcontrol_Vhard'

# read data
df_study <- read.csv('1_data/tmp_DEN/4_datasets/df_control_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### Tabove model and other data: control predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
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


#### Tabove model and other data: from below data selection ####

# set values of the case study
data_used <- 'Tbelow_Vhard'

# read data
df_study <- read.csv('1_data/tmp_DEN/4_datasets/df_above_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### Tabove model and other data: from below predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
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


#### Save results ####

# remove everything except metrics
rm(list=setdiff(ls(), "case_metrics"))

# save metrics
save.image('1_data/1_original_df/7_applications/case_metrics_above.RData')

# remove all
rm(list=ls())


#### +-+-+- Break to change the case +-+-+- ####


#### Load general information ####

# load the variables groups from the previous code
load('1_data/1_original_df/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, normalize, time_graph)


#### Tbelow model and other data: model selection ####

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
# my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tbelow_Vhard', ]
# my_models <- my_models[my_models$Metrics %in% 'mcc', ]
# 
# # I pick the models already trained
# load('1_data/3_final/5_analysis/Tbelow_Vhard/models.RData')
# 
# # select the models needed
# lr_m <- lr_model$lr_model_89
# dt_m <- dt_model$dt_model_7
# rf_m <- rf_model$rf_model_55
# nb_m <- nb_model$nb_model_84
# knn_m <- knn_model$knn_model_18
# svm_m <- svm_model$svm_model_11
# models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)
# 
# # and delete the rest to save space
# rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
#    lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# already selected models
models_list <- readRDS('1_data/1_original_df/7_applications/below_models.rds')
my_models <- readRDS('1_data/1_original_df/7_applications/below_models_summary.rds')

# read combis 
my_combis <- readRDS('1_data/tmp_DEN/4_datasets/combis_hard.RDS')


#### Tbelow model and other data: control data selection ####

# Normalization function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# create final df with metrics
case_metrics <- data.frame()

# set values of the case study
model_used <- 'Tbelow_Vhard'
data_used <- 'Tcontrol_Vhard'

# read data
df_study <- read.csv('1_data/tmp_DEN/4_datasets/df_control_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### Tbelow model and other data: control predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
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


#### Tbelow model and other data: from above data selection ####

# set values of the case study
data_used <- 'Tabove_Vhard'

# read data
df_study <- read.csv('1_data/tmp_DEN/4_datasets/df_above_time.csv')

# reorder variables to have dead always as the first one (important later)
df_study <- dplyr::select(df_study, dead, everything())

# divide data: 80% train and 20% test
smp_size <- floor(0.8 * nrow(df_study))

# set the seed to make your partition reproducible
set.seed(33)


#### Tbelow model and other data: from above predictions ####

# for each case
for(k in 2:length(names_methods)-1){
  
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


#### Save results ####

# remove everything except metrics
rm(list=setdiff(ls(), "case_metrics"))

# save metrics
save.image('1_data/1_original_df/7_applications/case_metrics_above.RData')

# remove all
rm(list=ls())
