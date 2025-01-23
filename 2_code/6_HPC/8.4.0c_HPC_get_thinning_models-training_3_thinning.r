#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                       Application on thinning - get models                         ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 06/11/2023                              #
#                              Last modification: 21/01/2025                               #
#------------------------------------------------------------------------------------------#



#### Select and save the best models for each application case study ####


setwd('/home/aitorvazquez/PhD/Vitality/3_final/data/7_applications/')


#### Control + above models ####


#### Load general information ####

# load the variables groups from the previous code
load('/home/aitorvazquez/PhD/Vitality/3_final/data/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, time_graph)



#### 3 thinning models ####


#### 3 thinning models: model selection ####

print('3 thinning models: model selection')

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Dbig_Vhard', ]
my_models <- my_models[my_models$Metrics %in% 'mcc', ]

# I pick the models already trained
load('/home/aitorvazquez/PhD/Vitality/3_final/data/5_analysis/Dbig_Vhard/models.RData')
print('all models loaded!')

# select the models needed
lr_m <- lr_model$lr_model_71
dt_m <- dt_model$dt_model_21
rf_m <- rf_model$rf_model_80
nb_m <- nb_model$nb_model36
knn_m <- knn_model$knn_model_27
svm_m <- svm_model$svm_model_81
models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# and delete the rest to save space
rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
   lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# save models
saveRDS(models_list, '3_thinning_models.rds')
saveRDS(my_models, '3_thinning_models_summary.rds')

print('3_thinning models ready to use.')