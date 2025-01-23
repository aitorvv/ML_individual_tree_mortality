#------------------------------------------------------------------------------------------#
####                       Application on thinning - comparison                         ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 06/11/2023                              #
#                              Last modification: 15/01/2025                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
library(ggplot2)
library(ggpubr)

setwd('/media/aitor/WDE/PhD_UVa/1_Topics/2_Vitality/')


#### Load general information: control thinning ####

# load case metrics
load('1_data/3_final/7_applications/case_metrics_control.RData')

# load all metrics
load('1_data/3_final/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, normalize, time_graph, all_timers)


#### Select metrics from original cases ####

# select metrics
original_metrics <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tcontrol_Vhard', ]
original_metrics <- original_metrics[original_metrics$Metrics %in% 'mcc', ]
original_metrics <- original_metrics[!original_metrics$names_methods %in% 'ANN', ]

# manage metrics content
original_metrics <- select(original_metrics, -best_model)
original_metrics <- rename(original_metrics, 
                           classifier = names_methods,
                           pred_acc = best_acc,
                           metric = Metrics, 
                           model_used = case)
original_metrics$data_used <- original_metrics$model_used

# merge them
metrics <- rbind(case_metrics, original_metrics)

# append to a general metrics df
all_thinning_metrics <- metrics


#### Graph comparison ####

# load graph functions
source('2_scripts/3_final/8.3_graph_functions.r')

# reorder the levels of the 'data' variable based on your preferred order
metrics$data_used <- ifelse(metrics$data_used == 'Tcontrol_Vhard', 'control', ifelse(metrics$data_used == 'Tbelow_Vhard', 'below', 'above'))
desired_order <- c("control", "above", "below") 

# reorder the levels of the 'data' variable in your dataframe
metrics$data_used <- factor(metrics$data_used, levels = desired_order)

# graph information
x_axis <- factor(metrics$classifier, levels = names_methods[-7])
y_axis <- as.numeric(metrics$pred_acc)
fill <- factor(metrics$classifier, levels = names_methods[-7])
min_shadow <- 0.5
max_shadow <- 1.5
g_title <- 'Performance comparison for the application of unthinned model using different datasets'
g_x <- 'Algorithm'
g_y <- 'MCC'
g_legend <- 'Algorithm'
color_groups <- color_methods[-7]
metrics$facet <- metrics$data_used
df_graph <- metrics
graph_title <- 'application/Tcontrol_to_others'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)
pl_control <- pointline_by_cs(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                graph_title = 'application/pointline-control_to_others', 
                min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)
pl_control_dashed <- pointline_by_cs_dashed(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                              graph_title = 'application/pointline-control_to_others', 
                              min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)
pl_control_point <- pointline_by_cs_just_point(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                                            graph_title = 'application/pointline-control_to_others', 
                                            min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)


#### +-+-+- Break to change the case +-+-+- ####


#### Load general information: from above thinning ####

# load case metrics
load('1_data/3_final/7_applications/case_metrics_above.RData')
case_metrics$data_used <- ifelse(case_metrics$data_used %in% 'control_Vhard', 'Tcontrol_Vhard', case_metrics$data_used)

#### Select metrics from original cases ####

# select metrics
original_metrics <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tabove_Vhard', ]
original_metrics <- original_metrics[original_metrics$Metrics %in% 'mcc', ]
original_metrics <- original_metrics[!original_metrics$names_methods %in% 'ANN', ]

# manage metrics content
original_metrics <- select(original_metrics, -best_model)
original_metrics <- rename(original_metrics, 
                           classifier = names_methods,
                           pred_acc = best_acc,
                           metric = Metrics, 
                           model_used = case)
original_metrics$data_used <- original_metrics$model_used

# merge them
metrics <- rbind(case_metrics, original_metrics)

# append to a general metrics df
all_thinning_metrics <- rbind(all_thinning_metrics, metrics)


#### Graph comparison ####

# reorder the levels of the 'data' variable based on your preferred order
metrics$data_used <- ifelse(metrics$data_used == 'Tcontrol_Vhard', 'control', ifelse(metrics$data_used == 'Tbelow_Vhard', 'below', 'above'))
desired_order <- c("control", "above", "below") 

# reorder the levels of the 'data' variable in your dataframe
metrics$data_used <- factor(metrics$data_used, levels = desired_order)

# graph information
x_axis <- factor(metrics$classifier, levels = names_methods[-7])
y_axis <- as.numeric(metrics$pred_acc)
fill <- factor(metrics$classifier, levels = names_methods[-7])
min_shadow <- 1.5
max_shadow <- 2.5
g_title <- 'Performance comparison for the application of thinning from above model using different datasets'
g_x <- 'Algorithm'
g_y <- 'MCC'
g_legend <- 'Algorithm'
color_groups <- color_methods[-7]
metrics$facet <- metrics$data_used
df_graph <- metrics
graph_title <- 'application/Tabove_to others'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)
pl_above <- pointline_by_cs(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                graph_title = 'application/pointline-above_to_others', 
                min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)
pl_above_dashed <- pointline_by_cs_dashed(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                            graph_title = 'application/pointline-above_to_others', 
                            min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)
pl_above_point <- pointline_by_cs_just_point(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                                          graph_title = 'application/pointline-above_to_others', 
                                          min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)


#### +-+-+- Break to change the case +-+-+- ####


#### Load general information: from below thinning ####

# load case metrics
load('1_data/3_final/7_applications/case_metrics_below.RData')
case_metrics$data_used <- ifelse(case_metrics$data_used %in% 'control_Vhard', 'Tcontrol_Vhard', case_metrics$data_used)

#### Select metrics from original cases ####

# select metrics
original_metrics <- all_cases_best_model_compilation[all_cases_best_model_compilation$case %in% 'Tbelow_Vhard', ]
original_metrics <- original_metrics[original_metrics$Metrics %in% 'mcc', ]
original_metrics <- original_metrics[!original_metrics$names_methods %in% 'ANN', ]

# manage metrics content
original_metrics <- select(original_metrics, -best_model)
original_metrics <- rename(original_metrics, 
                           classifier = names_methods,
                           pred_acc = best_acc,
                           metric = Metrics, 
                           model_used = case)
original_metrics$data_used <- original_metrics$model_used

# merge them
metrics <- rbind(case_metrics, original_metrics)

# append to a general metrics df
all_thinning_metrics <- rbind(all_thinning_metrics, metrics)


#### Graph comparison ####

# reorder the levels of the 'data' variable based on your preferred order
metrics$data_used <- ifelse(metrics$data_used == 'Tcontrol_Vhard', 'control', ifelse(metrics$data_used == 'Tbelow_Vhard', 'below', 'above'))
desired_order <- c("control", "above", "below") 

# reorder the levels of the 'data' variable in your dataframe
metrics$data_used <- factor(metrics$data_used, levels = desired_order)

# graph information
x_axis <- factor(metrics$classifier, levels = names_methods[-7])
y_axis <- as.numeric(metrics$pred_acc)
fill <- factor(metrics$classifier, levels = names_methods[-7])
min_shadow <- 2.5
max_shadow <- 3.5
g_title <- 'Performance comparison for the application of thinning from below model using different datasets'
g_x <- 'Algorithm'
g_y <- 'MCC'
g_legend <- 'Algorithm'
color_groups <- color_methods[-7]
metrics$facet <- metrics$data_used
df_graph <- metrics
graph_title <- 'application/Tbelow_to others'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)
pl_below <- pointline_by_cs(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                graph_title = 'application/pointline-below_to_others', 
                min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)
pl_below_dashed <- pointline_by_cs_dashed(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                            graph_title = 'application/pointline-below_to_others', 
                            min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)
pl_below_point <- pointline_by_cs_just_point(df_graph, x_axis = factor(metrics$data_used), y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, 
                                          graph_title = 'application/pointline-below_to_others', 
                                          min_shadow = min_shadow, max_shadow = max_shadow, y_min = 0, y_max = 0.8)


#### +-+-+- Break to change the case +-+-+- ####

#### Graph everything together ####

# rename models and data used
metrics <- all_thinning_metrics
metrics$model_used <- ifelse(metrics$model_used %in% 'Tcontrol_Vhard', 'control',
                             ifelse(metrics$model_used %in% 'Tbelow_Vhard', 'below', 'above'))
metrics$data_used <- ifelse(metrics$data_used %in% 'Tcontrol_Vhard', 'control',
                            ifelse(metrics$data_used %in% 'Tbelow_Vhard', 'below', 'above'))
metrics$description <- paste(metrics$model_used, ' model x ', metrics$data_used, ' data', sep = '')

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("control model x control data", "control model x above data", "control model x below data", 
                   "above model x control data", "above model x above data", "above model x below data",
                   "below model x control data", "below model x above data", "below model x below data") 

# reorder the levels of the 'data' variable in your dataframe
metrics$description <- factor(metrics$description, levels = desired_order)

# graph information
x_axis <- factor(metrics$classifier, levels = names_methods[-7])
y_axis <- as.numeric(metrics$pred_acc)
fill <- factor(metrics$classifier, levels = names_methods[-7])
g_title <- 'Performance comparison for models cross-application among thinning regimes'
g_x <- 'Algorithm'
g_y <- 'MCC'
g_legend <- 'Algorithm'
color_groups <- color_methods[-7]
metrics$facet <- metrics$description
df_graph <- metrics
graph_title <- 'application/all_cases'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)

pl_control <- pl_control + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.text = element_text(size = 16))
pl_above <- pl_above + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_blank(), legend.text = element_text(size = 16))
pl_below <- pl_below + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_blank(), legend.text = element_text(size = 16))
ggarrange(pl_control, pl_above, pl_below, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "B", "C"), hjust = -0.5, font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/application/pointline_all_cases.png", units = 'mm', dpi = 300, width = 600, height = 300)

pl_control_dashed <- pl_control_dashed + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.text = element_text(size = 16))
pl_above_dashed <- pl_above_dashed + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_blank(), legend.text = element_text(size = 16))
pl_below_dashed <- pl_below_dashed + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_blank(), legend.text = element_text(size = 16))
ggarrange(pl_control_dashed, pl_above_dashed, pl_below_dashed, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "B", "C"), hjust = -0.5, font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/application/pointline_all_cases_dashed.png", units = 'mm', dpi = 300, width = 600, height = 300)

pl_control_point <- pl_control_point + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.text = element_text(size = 16))
pl_above_point <- pl_above_point + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_blank(), legend.text = element_text(size = 16))
pl_below_point <- pl_below_point + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_blank(), legend.text = element_text(size = 16))
ggarrange(pl_control_point, pl_above_point, pl_below_point, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom", labels = c("A", "B", "C"), hjust = -0.5, font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/application/pointline_all_cases_point.png", units = 'mm', dpi = 300, width = 600, height = 300)
