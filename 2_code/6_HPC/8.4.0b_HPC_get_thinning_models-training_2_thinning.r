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
load('/home/aitorvazquez/PhD/Vitality/3_final/data/6_final_results/best_models-training_2_thinning.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, time_graph)


#### Control + above model and below data: model selection ####

print('Control + above model and below data: model selection')

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tcontrolabove_Vhard', ]
my_models <- my_models[my_models$Metrics %in% 'mcc', ]

# I pick the models already trained
load('/home/aitorvazquez/PhD/Vitality/3_final/data/5_analysis/Tcontrolabove_Vhard/models.RData')
print('all models loaded!')

# select the models needed
lr_m <- lr_model$lr_model_88
dt_m <- dt_model$dt_model_71
rf_m <- rf_model$rf_model_81
nb_m <- nb_model$nb_model_26
knn_m <- knn_model$knn_model_27
svm_m <- svm_model$svm_model_61
models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# and delete the rest to save space
rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
   lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# save models
saveRDS(models_list, 'controlabove_models.rds')
saveRDS(my_models, 'controlabove_models_summary.rds')

print('Control + above models ready to use.')



#### From control + below models ####


#### Control + below model and above data: model selection ####

print('Control + below model and above data: model selection')

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tcontrolbelow_Vhard', ]
my_models <- my_models[my_models$Metrics %in% 'mcc', ]

# I pick the models already trained
load('/home/aitorvazquez/PhD/Vitality/3_final/data/5_analysis/Tcontrolbelow_Vhard/models.RData')
print('all models loaded!')

# select the models needed
lr_m <- lr_model$lr_model_70
dt_m <- dt_model$dt_model_8
rf_m <- rf_model$rf_model_52
nb_m <- nb_model$nb_model_18
knn_m <- knn_model$knn_model_27
svm_m <- svm_model$svm_model_79
models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# and delete the rest to save space
rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
   lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# save models
saveRDS(models_list, 'controlbelow_models.rds')
saveRDS(my_models, 'controlbelow_models_summary.rds')

print('Control + below models ready to use.')


#### From above + below models ####


#### Above + below model and control data: model selection ####

print('Above + below model and control data: model selection')

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tabovebelow_Vhard', ]
my_models <- my_models[my_models$Metrics %in% 'mcc', ]

# I pick the models already trained
load('/home/aitorvazquez/PhD/Vitality/3_final/data/5_analysis/Tabovebelow_Vhard/models.RData')
print('all models loaded!')

# select the models needed
lr_m <- lr_model$lr_model_71
dt_m <- dt_model$dt_model_27
rf_m <- rf_model$rf_model_45
nb_m <- nb_model$nb_model_9
knn_m <- knn_model$knn_model_9
svm_m <- svm_model$svm_model_74
models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# and delete the rest to save space
rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
   lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# save models
saveRDS(models_list, 'abovebelow_models.rds')
saveRDS(my_models, 'abovebelow_models_summary.rds')

print('Above + below models ready to use.')

