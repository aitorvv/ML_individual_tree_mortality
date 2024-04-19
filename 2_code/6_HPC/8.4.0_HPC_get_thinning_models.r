#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                       Application on thinning - get models                         ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 06/11/2023                              #
#                              Last modification: 06/11/2023                               #
#------------------------------------------------------------------------------------------#



#### Select and save the best models for each application case study ####


setwd('/home/aitorvazquez/PhD/Vitality/3_final/data/7_applications/')


#### Control models ####


#### Load general information ####

# load the variables groups from the previous code
load('/home/aitorvazquez/PhD/Vitality/3_final/data/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, normalize, time_graph)


#### Unthinned model and thinned data: model selection ####

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tcontrol_Vhard', ]
my_models <- my_models[my_models$Metrics %in% 'mcc', ]

# I pick the models already trained
load('/home/aitorvazquez/PhD/Vitality/3_final/data/5_analysis/Tcontrol_Vhard/models.RData')

# select the models needed
lr_m <- lr_model$lr_model_27
dt_m <- dt_model$dt_model_44
rf_m <- rf_model$rf_model_7
nb_m <- nb_model$nb_model_7
knn_m <- knn_model$knn_model_26
svm_m <- svm_model$svm_model_34
models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# and delete the rest to save space
rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
   lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# save models
saveRDS(models_list, 'control_models.rds')
saveRDS(my_models, 'control_models_summary.rds')

print('Control models ready to use.')


#### From above models ####


#### Tabove model and other data: model selection ####

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tabove_Vhard', ]
my_models <- my_models[my_models$Metrics %in% 'mcc', ]

# I pick the models already trained
load('/home/aitorvazquez/PhD/Vitality/3_final/data/5_analysis/Tabove_Vhard/models.RData')

# select the models needed
lr_m <- lr_model$lr_model_79
dt_m <- dt_model$dt_model_18
rf_m <- rf_model$rf_model_70
nb_m <- nb_model$nb_model_75
knn_m <- knn_model$knn_model_3
svm_m <- svm_model$svm_model_81
models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# and delete the rest to save space
rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
   lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# save models
saveRDS(models_list, 'above_models.rds')
saveRDS(my_models, 'above_models_summary.rds')

print('Above models ready to use.')


#### From below models ####


#### Tbelow model and other data: model selection ####

# after inspect previous results, I choose the models with the "hard" type of variables
# I filter the best model to know which sould I use
my_models <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tbelow_Vhard', ]
my_models <- my_models[my_models$Metrics %in% 'mcc', ]

# I pick the models already trained
load('/home/aitorvazquez/PhD/Vitality/3_final/data/5_analysis/Tbelow_Vhard/models.RData')

# select the models needed
lr_m <- lr_model$lr_model_89
dt_m <- dt_model$dt_model_7
rf_m <- rf_model$rf_model_55
nb_m <- nb_model$nb_model_84
knn_m <- knn_model$knn_model_18
svm_m <- svm_model$svm_model_11
models_list <- list(lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# and delete the rest to save space
rm(lr_model, dt_model, rf_model, nb_model, knn_model, svm_model, 
   lr_m, dt_m, rf_m, nb_m, knn_m, svm_m)

# save models
saveRDS(models_list, 'below_models.rds')
saveRDS(my_models, 'below_models_summary.rds')

print('Below models ready to use.')