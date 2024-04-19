#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                              Metrics organization                                  ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 09/06/2023                              #
#                              Last modification: 09/01/2024                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
#library(naivebayes) # NV model
#library(ggpubr)
#library(mvnormtest) # mshapiro_test()
#library(corrplot) # correlation features plot
#library(caret) # easy ML workflow
library(ROCR)  # plot model performance
#library(leaps) # stepwise
#library(MASS) # AIC to choose the best model
#library(class) # KNN model
#library(randomForest) # RF
#library(e1071) # SVM
#library(neuralnet) # ANN
library(irr) # kappa
library(RJSONIO) # read results from Python neural networks
library(tictoc) # check time for training each model
library(reshape2) # change structure of data

setwd('/home/aitorvazquez/PhD/Vitality')


#### Selection section ####

# that part of the code is importart to perform the rest in a automatic way

# different options for all the case study analyzed
df_size = c('small', 'small_random', 'medium', 'medium_random', 'big')
thinning = c('control', 'below', 'above')
df_len = c('3to5', '5to6', '6to7', '7to9')
var_size = c('easy', 'medium', 'hard', 'extreme')

# case study 1 - different dataset sizes with different amount of variables
case_study_1 <- expand.grid(df_size, var_size)
case_study_1$name <- paste('D', case_study_1$Var1, '_V', case_study_1$Var2, sep = '')

# case study 2 - different thinning datasets with different amount of variables
case_study_2 <- expand.grid(thinning, var_size)
case_study_2$name <- paste('T', case_study_2$Var1, '_V', case_study_2$Var2, sep = '')

# case study 3 - different thinning datasets with different amount of variables
case_study_3 <- expand.grid(df_len, var_size)
case_study_3$name <- paste('L', case_study_3$Var1, '_V', case_study_3$Var2, sep = '')

# merge both
case_study <- rbind(case_study_1, case_study_2, case_study_3)

# remove temporal variables
rm(df_size, thinning, var_size, case_study_1, case_study_2, case_study_3, df_len)

# save case_study combinations
save.image('3_final/data/6_final_results/case_study_summary.RData')
print('Case study combinations saved')


#### Functions needed ####

# Normalization function
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

# 
# #### Import R analysis results ####
# 
# for(case in 1:length(case_study$name)){
#   
#   # Loading the workspace
#   load(paste("3_final/data/5_analysis/", case_study$name[case], "/metrics.RData", sep = ''))
#   
#   
#   #### Import results of ANN and continue analysis ####
#   
#   # divide data: 80% train and 20% test
#   smp_size <- floor(0.8 * nrow(df_study))
#   
#   # activate timer
#   tic("ANN_2")
#   
#   # model number
#   n_model = 0
#   
#   # lists: models and accuracy
#   ann_accuracy <- list()
#   ann_accuracy_dead <- list()
#   ann_accuracy_alive <- list()
#   ann_roc <- list()
#   ann_auc <- list()
#   ann_aucpr <- list()
#   ann_mcc <- list()
#   ann_kappa <- list()
#   ann_threshold <- list()
#   
#   for (k in my_combis){
#     
#     # model name
#     n_model = n_model + 1
#     name <- paste('ann_model_', n_model, sep = '')
#     
#     # get my variables combination
#     df <- df_study[, which((names(df_study) %in% k) == TRUE)]
#     
#     # normalize data
#     dead <- df$dead
#     df <- normalize(df[2:length(df)])
#     df$dead <- dead
#     
#     # split data
#     train_ind <- sample(seq_len(nrow(df)), size = smp_size)
#     train <- df[train_ind, ]
#     test <- df[-train_ind, ]
#     labels <- ifelse(test$dead == 'yes', 1, 0)
#     test <- dplyr::select(test, -dead)
#     #train$dead <- ifelse(train$dead == 'yes', 1, 0)
#     #test$dead <- ifelse(test$dead == 'yes', 1, 0)
#     
#     # get prediction data
#     predicions_path <- paste('3_final/data/5_analysis/ann/preds/', case_study$name[case], '/', name, '.json', sep = '')
#     pred_ann_model <- RJSONIO::fromJSON(predicions_path)
#     
#     # change format
#     pred_ann_model <- tibble(t(as.data.frame(pred_ann_model)))
#     pred_ann_model <- dplyr::rename(pred_ann_model, V1 = `t(as.data.frame(pred_ann_model))`)
#     
#     # choose the best threshold based on MCC
#     t_mcc <- 0
#     threshold <- 0
#     for(t in seq(0.001, 0.999, by = 0.001)){
#       t_pred <- ifelse(pred_ann_model$V1 > t, 1, 0)
#       t_pred_vs_real <- ROCR::prediction(t_pred, labels)
#       t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
#       if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
#          t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
#         t_mcc <- t_mcc_tmp@y.values[[1]][2]
#         threshold <- t
#       } 
#     }
#     
#     # record threshold information
#     ann_threshold[[name]] <- threshold
#     
#     # binary classify predictions
#     pred <- ifelse(pred_ann_model$V1 > threshold, 1, 0)
#     
#     # ROC curve, plotting TPR (True Positive Rate) against the false positive rate (FPR) 
#     pred_vs_real <- ROCR::prediction(pred, labels)
#     performance <- ROCR::performance(pred_vs_real, measure = "tpr", x.measure = "fpr")
#     ann_roc[[name]] <- performance
#     
#     # AUC is the Area Under the Curve - TPR vs FPR 
#     auc <- ROCR::performance(pred_vs_real, measure = "auc")
#     auc <- auc@y.values[[1]]
#     ann_auc[[name]] <- auc
#     
#     # AUCPR - TPR vs PPV - Precision/Recall
#     aucpr <- ROCR::performance(pred_vs_real, measure = "aucpr")
#     aucpr <- aucpr@y.values[[1]]
#     ann_aucpr[[name]] <- aucpr
#     
#     # MCC - Matthews correlation coefficient or phi coefficient
#     mcc <- ROCR::performance(pred_vs_real, measure = "phi")
#     mcc <- mcc@y.values[[1]]
#     ann_mcc[[name]] <- mcc
#     
#     # Cohen's kappa coefficient 
#     kappa <- kappa2(cbind(pred, labels))
#     ann_kappa[[name]] <- kappa
#     
#     # check accuracy
#     pred_errors <- mean(pred != labels)
#     ann_accuracy[[name]] <- 1 - pred_errors
#     
#     # calculate accuracy splitting on dead and alive trees
#     len_alive <- 0
#     right_alive <- 0
#     len_dead <- 0
#     right_dead <- 0
#     for (k in 1:length(labels)){
#       # depending if the tree originally was alive (0) or dead (1)
#       if(labels[k] == 0){
#         # calculate prediction on alive trees: 1 ok, 0 error
#         pred_alive <- ifelse(pred[k] == 0, 1, 0)
#         right_alive <- right_alive + as.numeric(pred_alive)
#         len_alive <- len_alive + 1
#       } else {
#         # calculate prediction on dead trees: 1 ok, 0 error
#         pred_dead <- ifelse(pred[k] == 1, 1, 0)
#         right_dead <- right_dead + as.numeric(pred_dead)
#         len_dead <- len_dead + 1
#       }
#     }
#     # add information to the main list 
#     ann_accuracy_alive[[name]] <- right_alive / len_alive
#     ann_accuracy_dead[[name]] <- right_dead / len_dead
#   }
#   
#   # exclude temporal variables
#   rm(test, train, k, labels, len_alive, len_dead, predicions_path, auc,
#      n_model, name, pred_alive, pred_dead, pred_errors, dead, right_alive,
#      right_dead, train_ind, performance, pred_ann_model, pred_vs_real, smp_size,
#      threshold, kappa, mcc, aucpr, t, t_mcc, pred, t_pred, t_pred_vs_real, t_mcc_tmp)
#   
#   # save time data  
#   my_timer <- rbind(my_timer, toc())
#   
#   
#   #### Import time records for ANN ####
#   
#   # read time preparing data (R)
#   ann_time_r <- read.csv(paste('3_final/data/5_analysis/ann/timer/', case_study$name[case], '/ANN_1_timer.csv', sep = ''))
#   
#   # append to timer
#   my_timer <- rbind(my_timer, ann_time_r)
#   
#   # read time of ANN performance (Python)
#   ann_time_path_py <- paste('3_final/data/5_analysis/ann/timer/', case_study$name[case], '/ann_time_list.json', sep = '')
#   ann_time_python <- RJSONIO::fromJSON(ann_time_path_py)
#   
#   # model number
#   n_model = 0
#   
#   # for each combi
#   for (k in 1:length(my_combis)){
#     
#     # model name
#     n_model = n_model + 1
#     name <- paste('ann_model_', n_model, sep = '')
#     
#     # organize data
#     tmp <- data.frame(tic = 0, toc = ann_time_python[[k]], msg = name, callback_msg = ann_time_python[[k]])
#     
#     # append to timer
#     my_timer <- rbind(my_timer, tmp)
#   }
#   
#   # create a new timer record grouping all the ANN steps
#   timers_ann <- my_timer[grep('ann', tolower(my_timer$msg)), ]
#   
#   # organize it to append
#   tmp <- data.frame(tic = 0, toc = sum(timers_ann$toc - timers_ann$tic), msg = 'ANN', 
#                     callback_msg = paste('ANN: ', sum(timers_ann$toc - timers_ann$tic), ' sec elapsed', sep = ''))
#   
#   # save time data
#   my_timer <- rbind(my_timer, tmp)
#   
#   # remove temporal variables
#   rm(ann_time_r, ann_time_path_py, ann_time_python, n_model, name, tmp, timers_ann, k)
#   
#   
#   #### Checkpoint ####
#   
#   # save metrics compilation
#   save.image(paste('3_final/data/6_final_results/', case_study$name[case], '/final_metrics.RData', sep = ''))
#   print(paste(case_study$name[case], ' saved succesfully!', sep = ''))
#   
#   # remove everything except needed variables
#   rm(list = setdiff(ls(), c('case_study', 'normalize')))
# }


#### Import R analysis results - adapted without ANN ####

for(case in 1:length(case_study$name)){
  
  # Loading the workspace
  load(paste("3_final/data/5_analysis/", case_study$name[case], "/metrics.RData", sep = ''))
  
  
  #### Checkpoint ####
  
  # save metrics compilation
  save.image(paste('3_final/data/6_final_results/', case_study$name[case], '/final_metrics.RData', sep = ''))
  print(paste(case_study$name[case], ' saved succesfully!', sep = ''))
  
  # remove everything except needed variables
  rm(list = setdiff(ls(), c('case_study', 'normalize')))
}

