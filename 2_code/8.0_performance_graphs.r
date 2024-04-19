#------------------------------------------------------------------------------------------#
####                               Performance graphs                                   ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 09/06/2023                              #
#                              Last modification: 04/01/2024                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
library(reshape2) # change structure of data
library(ggplot2)
library(ggdist)

setwd('ML_individual_tree_mortality/')


#### Load general information ####

# load the variables groups from the previous code
load('1_data/1_original_df/6_final_results/case_study_summary.RData')

# load functions to graph
source('2_code/8.1_functions_performance_graphs.r')

# df to compare the best model among study cases
all_cases_best_model_compilation <- tibble()
all_timers <- tibble()


#### Graphs results and choose best model ####

# loop to graph all the results case by case and select the best models of each case study

for(case in 1:length(case_study$name)){
  
  # Loading the workspace
  load(paste('1_data/1_original_df/6_final_results/', case_study$name[case], '/final_metrics.RData', sep = ''))
  
  
  #### Models comparison function ####
  
  # common information
  n_data <- length(my_combis)
  names_methods <- c('LR', 'DT', 'RF', 'NB', 'KNN', 'SVM')  #, 'ANN')
  color_methods <- c('gray', 'darkolivegreen', 'darkgreen', 'darkblue', 'darkred', 'darkorange')  #, 'gold')
  
  #### Accuracy alive trees ####
  
  metric_list <- list(lr_accuracy_alive, dt_accuracy_alive, rf_accuracy_alive, nb_accuracy_alive, 
                      knn_accuracy_alive, svm_accuracy_alive)  #, ann_accuracy_alive)
  g_title <- paste('Binary classification performance for ', n_data, ' models tested: accuracy predicting alive trees', sep = '')
  g_x <- ''
  g_y <- 'Accuracy alive trees'
  g_legend <- 'Classifier'
  g_name <- 'acc_alive'
  metric_graph(metrics_list = metric_list, n_data = n_data, names_methods = names_methods, 
               color_methods = color_methods, g_title = g_title, g_x = g_x, g_y = g_y, g_legend = g_legend,
               g_name = g_name, case_study_name = case_study$name[case])
  
  
  #### Accuracy dead trees ####
  
  metric_list <- list(lr_accuracy_dead, dt_accuracy_dead, rf_accuracy_dead, nb_accuracy_dead, 
                      knn_accuracy_dead, svm_accuracy_dead)  #, ann_accuracy_dead)
  g_title <- paste('Binary classification performance for ', n_data, ' models tested: accuracy predicting dead trees', sep = '')
  g_x <- ''
  g_y <- 'Accuracy dead trees'
  g_legend <- 'Classifier'
  g_name <- 'acc_dead'
  metric_graph(metrics_list = metric_list, n_data = n_data, names_methods = names_methods, 
               color_methods = color_methods, g_title = g_title, g_x = g_x, g_y = g_y, g_legend = g_legend,
               g_name = g_name, case_study_name = case_study$name[case])
  
  
  #### Accuracy total trees ####
  
  metric_list <- list(lr_accuracy, dt_accuracy, rf_accuracy, nb_accuracy, 
                      knn_accuracy, svm_accuracy)  #, ann_accuracy)
  g_title <- paste('Binary classification performance for ', n_data, ' models tested: accuracy predicting all trees', sep = '')
  g_x <- ''
  g_y <- 'Accuracy all trees'
  g_legend <- 'Classifier'
  g_name <- 'acc'
  metric_graph(metrics_list = metric_list, n_data = n_data, names_methods = names_methods, 
               color_methods = color_methods, g_title = g_title, g_x = g_x, g_y = g_y, g_legend = g_legend,
               g_name = g_name, case_study_name = case_study$name[case])
  
  
  #### AUC ####
  
  metric_list <- list(lr_auc, dt_auc, rf_auc, nb_auc, 
                      knn_auc, svm_auc)  #, ann_auc)
  g_title <- paste('Binary classification performance for ', n_data, ' models tested: Area Under the Curve (AUC)', sep = '')
  g_x <- ''
  g_y <- 'AUC'
  g_legend <- 'Classifier'
  g_name <- 'auc'
  metric_graph(metrics_list = metric_list, n_data = n_data, names_methods = names_methods, 
               color_methods = color_methods, g_title = g_title, g_x = g_x, g_y = g_y, g_legend = g_legend,
               g_name = g_name, case_study_name = case_study$name[case])
  
  
  #### AUCPR ####
  
  metric_list <- list(lr_aucpr, dt_aucpr, rf_aucpr, nb_aucpr, 
                      knn_aucpr, svm_aucpr)  #, ann_aucpr)
  g_title <- paste('Binary classification performance for ', n_data, ' models tested: Area Under the Precision/Recall Curve (AUCPR)', sep = '')
  g_x <- ''
  g_y <- 'AUCPR'
  g_legend <- 'Classifier'
  g_name <- 'aucpr'
  metric_graph(metrics_list = metric_list, n_data = n_data, names_methods = names_methods, 
               color_methods = color_methods, g_title = g_title, g_x = g_x, g_y = g_y, g_legend = g_legend,
               g_name = g_name, case_study_name = case_study$name[case])
  
  
  #### KAPPA ####
  lr_k2 = dt_k2 = rf_k2 = nb_k2 = knn_k2 = svm_k2 = 0  #ann_k2 = 0
  for(k in 1:n_data){
    lr_k2[[k]] <- lr_kappa[[k]]$value
    dt_k2[[k]] <- dt_kappa[[k]]$value
    rf_k2[[k]] <- rf_kappa[[k]]$value
    nb_k2[[k]] <- nb_kappa[[k]]$value
    knn_k2[[k]] <- knn_kappa[[k]]$value
    svm_k2[[k]] <- svm_kappa[[k]]$value
    #ann_k2[[k]] <- ann_kappa[[k]]$value
  }
  
  metric_list <- list(lr_k2, dt_k2, rf_k2, nb_k2, knn_k2, svm_k2)  #, ann_k2)
  g_title <- paste('Binary classification performance for ', n_data, " models tested: Cohen's Kappa", sep = '')
  g_x <- ''
  g_y <- 'KAPPA'
  g_legend <- 'Classifier'
  g_name <- 'kappa'
  metric_graph(metrics_list = metric_list, n_data = n_data, names_methods = names_methods, 
               color_methods = color_methods, g_title = g_title, g_x = g_x, g_y = g_y, g_legend = g_legend,
               g_name = g_name, case_study_name = case_study$name[case])
  
  
  #### MCC ####
  lr_mcc2 = dt_mcc2 = rf_mcc2 = nb_mcc2 = knn_mcc2 = svm_mcc2 = 0  #ann_mcc2 = 0
  for(k in 1:n_data){
    lr_mcc2[[k]] <- lr_mcc[[k]][[2]]
    dt_mcc2[[k]] <- dt_mcc[[k]][[2]]
    rf_mcc2[[k]] <- rf_mcc[[k]][[2]]
    nb_mcc2[[k]] <- nb_mcc[[k]][[2]]
    knn_mcc2[[k]] <- knn_mcc[[k]][[2]]
    svm_mcc2[[k]] <- svm_mcc[[k]][[2]]
    #ann_mcc2[[k]] <- ann_mcc[[k]][[2]]
  }
  
  metric_list <- list(lr_mcc2, dt_mcc2, rf_mcc2, nb_mcc2, knn_mcc2, svm_mcc2)  #, ann_mcc2)
  g_title <- paste('Binary classification performance for ', n_data, ' models tested: Phi Coefficient or Matthews Correlation Coefficient (MCC)', sep = '')
  g_x <- ''
  g_y <- 'MCC'
  g_legend <- 'Classifier'
  g_name <- 'mcc'
  metric_graph(metrics_list = metric_list, n_data = n_data, names_methods = names_methods, 
               color_methods = color_methods, g_title = g_title, g_x = g_x, g_y = g_y, g_legend = g_legend,
               g_name = g_name, case_study_name = case_study$name[case])
  
  
  #### Time performance ####
  
  my_timer$classifier <- sub('_model*.', '', my_timer$msg)
  my_timer$seconds <- my_timer$toc - my_timer$tic
  my_timer$mins <- my_timer$seconds/60
  my_timer$hours <- my_timer$mins/60
  
  # add informations about number of features
  n <- length(names(my_combis))
  count <- 1
  my_timer$n_vars <- ''
  my_timer$code_model <- ''
  model_list <- c('lr_model_', 'dt_model_', 'rf_model_', 'nb_model_', 'knn_model_', 'svm_model_')  #, 'ann_model_')
  
  # iterate over all models to get the number of features and the code model
  for(i in 1:n){
    my_timer$n_vars <- ifelse(my_timer$msg %in% paste(model_list, count, sep = ''), length(my_combis[[i]]), my_timer$n_vars)
    my_timer$code_model <- ifelse(my_timer$msg %in% paste(model_list, count, sep = ''), names(my_combis[i]), my_timer$code_model)
    count <- count + 1
  }

  my_timer_general <- my_timer[my_timer$msg %in% names_methods, ]
  
  my_order <- c("LR", "DT", "RF", "NB", "KNN", "SVM")  #, "ANN")
  
  time_graph(my_timer_general = my_timer_general, 
             my_order = my_order,
             case_study_name = case_study$name[case])
  
  
  #### Best feature combination for each model ####
  
  # General accuracy
  all_methods_acc <- list(lr_accuracy, dt_accuracy, rf_accuracy, nb_accuracy, 
                          knn_accuracy, svm_accuracy)  #, ann_accuracy)
  best_accuracy <- find_best_model(names_methods = names_methods, # methods names
                                   all_methods_acc = all_methods_acc, # list with all the metrics of each method
                                   my_combis = my_combis) # features combinations
  
  # Alive trees accuracy
  all_methods_acc_alive <- list(lr_accuracy_alive, dt_accuracy_alive, rf_accuracy_alive, 
                                nb_accuracy_alive, knn_accuracy_alive, svm_accuracy_alive)  #, ann_accuracy_alive)
  best_accuracy_alive <- find_best_model(names_methods = names_methods, # methods names
                                         all_methods_acc = all_methods_acc_alive, # list with all the metrics of each method
                                         my_combis = my_combis) # features combinations
  
  # Dead trees accuracy
  all_methods_acc_dead <- list(lr_accuracy_dead, dt_accuracy_dead, rf_accuracy_dead, 
                               nb_accuracy_dead, knn_accuracy_dead, svm_accuracy_dead)  #, ann_accuracy_dead)
  best_accuracy_dead <- find_best_model(names_methods = names_methods, # methods names
                                        all_methods_acc = all_methods_acc_dead, # list with all the metrics of each method
                                        my_combis = my_combis) # features combinations
  
  # AUC
  all_methods_auc <- list(lr_auc, dt_auc, rf_auc, nb_auc, 
                          knn_auc, svm_auc)  #, ann_auc)
  best_auc <- find_best_model(names_methods = names_methods, # methods names
                              all_methods_acc = all_methods_auc, # list with all the metrics of each method
                              my_combis = my_combis) # features combinations
  
  # AUCPR
  all_methods_aucpr <- list(lr_aucpr, dt_aucpr, rf_aucpr, nb_aucpr, 
                            knn_aucpr, svm_aucpr)  #, ann_aucpr)
  best_aucpr <- find_best_model(names_methods = names_methods, # methods names
                                all_methods_acc = all_methods_aucpr, # list with all the metrics of each method
                                my_combis = my_combis) # features combinations
  
  # KAPPA
  all_methods_kappa <- all_methods_acc
  for(k in 1:length(my_combis)){
    all_methods_kappa[[1]][k] <- ifelse(lr_kappa[[k]]$value != 'NaN', lr_kappa[[k]]$value, 0)
    all_methods_kappa[[2]][k] <- ifelse(dt_kappa[[k]]$value != 'NaN', dt_kappa[[k]]$value, 0)
    all_methods_kappa[[3]][k] <- ifelse(rf_kappa[[k]]$value != 'NaN', rf_kappa[[k]]$value, 0)
    all_methods_kappa[[4]][k] <- ifelse(nb_kappa[[k]]$value != 'NaN', nb_kappa[[k]]$value, 0)
    all_methods_kappa[[5]][k] <- ifelse(knn_kappa[[k]]$value != 'NaN', knn_kappa[[k]]$value, 0)
    all_methods_kappa[[6]][k] <- ifelse(svm_kappa[[k]]$value != 'NaN', svm_kappa[[k]]$value, 0)
    #all_methods_kappa[[7]][k] <- ifelse(ann_kappa[[k]]$value != 'NaN', ann_kappa[[k]]$value, 0)
  }
  best_kappa <- find_best_model(names_methods = names_methods, # methods names
                                all_methods_acc = all_methods_kappa, # list with all the metrics of each method
                                my_combis = my_combis) # features combinations
  
  # MCC
  all_methods_mcc <- all_methods_acc
  for(k in 1:length(my_combis)){
    all_methods_mcc[[1]][k] <- ifelse(lr_mcc[[k]][2] != 'NaN', lr_mcc[[k]][2], 0)
    all_methods_mcc[[2]][k] <- ifelse(dt_mcc[[k]][2] != 'NaN', dt_mcc[[k]][2], 0)
    all_methods_mcc[[3]][k] <- ifelse(rf_mcc[[k]][2] != 'NaN', rf_mcc[[k]][2], 0)
    all_methods_mcc[[4]][k] <- ifelse(nb_mcc[[k]][2] != 'NaN', nb_mcc[[k]][2], 0)
    all_methods_mcc[[5]][k] <- ifelse(knn_mcc[[k]][2] != 'NaN', knn_mcc[[k]][2], 0)
    all_methods_mcc[[6]][k] <- ifelse(svm_mcc[[k]][2] != 'NaN', svm_mcc[[k]][2], 0)
    #all_methods_mcc[[7]][k] <- ifelse(ann_mcc[[k]][2] != 'NaN', ann_mcc[[k]][2], 0)
  }
  best_mcc <- find_best_model(names_methods = names_methods, # methods names
                              all_methods_acc = all_methods_mcc, # list with all the metrics of each method
                              my_combis = my_combis) # features combinations
  
  
  #### Graph of all best models ####
  
  # group df and plot again
  best_accuracy$Metrics <- 'accuracy'
  best_accuracy_alive$Metrics <- 'accuracy_alive'
  best_accuracy_dead$Metrics <- 'accuracy_dead'
  best_auc$Metrics <- 'auc'
  best_aucpr$Metrics <- 'aucpr'
  best_kappa$Metrics <- 'kappa'
  best_mcc$Metrics <- 'mcc'
  best_compilation <- rbind(best_accuracy, best_accuracy_alive, best_accuracy_dead,
                            best_auc, best_aucpr, best_kappa, best_mcc)
  
  # set the name of the case study
  best_compilation$case <- case_study$name[case]
  
  # reorder
  best_compilation$names_methods <- factor(best_compilation$names_methods, 
                                           levels = names_methods)
  
  # graph results
  graph_best_model(best_compilation = best_compilation, case_study_name = case_study$name[case])
  
  
  #### Clear environment ####
  
  # save best model metrics
  all_cases_best_model_compilation <- rbind(all_cases_best_model_compilation, best_compilation)
  
  # save time spent into perform analysis
  my_timer$case <- case_study$name[case]
  all_timers <- rbind(all_timers, my_timer)
  
  # notify and remove not needed variables
  print(paste(case_study$name[case], ' case study finished.', sep = ''))
  rm(list = setdiff(ls(), c('all_cases_best_model_compilation', 'find_best_model', 'graph_best_model', 'metric_graph', 
                            'normalize', 'time_graph', 'case_study', 'names_methods', 'color_methods', 'all_timers')))

}


#### Checkpoint ####

save.image('1_data/1_original_df/6_final_results/best_models.RData')
# load('1_data/3_final/6_final_results/best_models.RData')
# 
# 
# 
# 
# # quería ordenarlos y mostrar bien los resultados para cada caso e estudio
# # quizás harbía que repetir ANN o hacer más casos de estudio
# # RF parece el mejor siempre
# 
# 
# 
# 
# #names_methods <- c('LR', 'DT', 'RF', 'NB', 'KNN', 'SVM', 'ANN')
# #color_methods <- c('gray', 'darkolivegreen', 'darkgreen', 'darkblue', 'darkred', 'darkorange', 'gold')
# 
# 
# #### Comparison among study cases: case study 1 ####
# 
# # split case study 1 data
# case_1 <- all_cases_best_model_compilation[grep('D', all_cases_best_model_compilation$case), ]
# case_2 <- all_cases_best_model_compilation[grep('T', all_cases_best_model_compilation$case), ]
# 
# # graph results per study case and metric
# 
# for(metric in unique(case_1$Metrics)){
#   case_1_metric <- case_1[case_1$Metrics == 'mcc', ]
# }
# case_1_metric$best_acc <- as.numeric(case_1_metric$best_acc)
# 
# 
# for(metric in unique(case_2$Metrics)){
#   case_2_metric <- case_2[case_2$Metrics == 'mcc', ]
# }
# case_2_metric$best_acc <- as.numeric(case_2_metric$best_acc)
# 
# cases_order <- c("Dsmall_Veasy", "Dsmall_Vmedium", "Dsmall_Vhard", "Dmedium_Veasy", "Dmedium_Vmedium", "Dmedium_Vhard", "Dbig_Veasy", "Dbig_Vmedium", "Dbig_Vhard")
# 
# 
# 
# # Create a ggplot with only density plots splitted by classifier
# ggplot(case_1_metric, aes(x = best_acc, colour = names_methods)) + 
#   geom_bar(lwd = 0.5, linetype = 1) + # graph type
#   labs(title = 'g_title', # title
#        x = 'g_x', y = 'g_y', # labs
#        fill = 'g_legend') + # legend
#   theme_light() + # theme
#   theme(plot.title = element_text(hjust = 0.5)) + # center title
#   scale_color_manual(values = color_methods) + #c(color_methods, 'darkviolet')) +  # colors
#   facet_wrap(~ case, scales = 'free')
# 
# ggplot(case_1_metric, aes(x = best_acc, colour = names_methods)) + 
#   geom_density(lwd = 0.5, linetype = 1) + # graph type
#   labs(title = 'g_title', # title
#        x = 'g_x', y = 'g_y', # labs
#        fill = 'g_legend') + # legend
#   theme_light() + # theme
#   theme(plot.title = element_text(hjust = 0.5)) + # center title
#   scale_color_manual(values = color_methods)  #c(color_methods, 'darkviolet')) +  # colors
#   
# 
# #------------------------------------
# 
# # Create a grouped bar chart
# grouped_bar_chart <- ggplot(case_1_metric, aes(x = factor(case), y = best_acc, fill = names_methods)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Best model selected per classifier across case study: data splitted by size",
#        x = "Case study",
#        y = "MCC") +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   scale_fill_manual(values = color_methods)  # Custom color scheme for methodologies
# 
# print(grouped_bar_chart)
# # save graph
# my_path <- paste('2_scripts/4_figures/9.4_best_model_metrics/classifiers_mcc_size.png', sep = '')
# ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
# 
# 
# # Create a grouped bar chart
# grouped_bar_chart <- ggplot(case_2_metric, aes(x = factor(case), y = best_acc, fill = names_methods)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Best model selected per classifier across case study: data splitted by thinning regime",
#        x = "Case study",
#        y = "MCC") +
#   theme_minimal() +
#   theme(legend.position = "top") +
#   scale_fill_manual(values = color_methods)  # Custom color scheme for methodologies
# 
# print(grouped_bar_chart)
# # save graph
# my_path <- paste('2_scripts/4_figures/9.4_best_model_metrics/classifiers_mcc_thinning.png', sep = '')
# ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
