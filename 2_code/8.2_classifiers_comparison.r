#------------------------------------------------------------------------------------------#
####                              Case studies: analysis                                ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 27/10/2023                              #
#                              Last modification: 07/11/2023                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
library(reshape2) # change structure of data
library(rstatix)
library(ggpubr) 
library(ggplot2)
library(ggdist)
library(ggridges)

setwd('ML_individual_tree_mortality/')


#### Load general information ####

# load the variables groups from the previous code
load('1_data/1_original_df/6_final_results/best_models.RData')

# remove functions
rm(find_best_model, graph_best_model, metric_graph, normalize, time_graph)


#### Data management: performance ####

# filter just one metric to evaluate
df <- all_cases_best_model_compilation[all_cases_best_model_compilation$Metrics %in% 'mcc', ]
df$best_acc <- as.numeric(df$best_acc)

# split case study in 3 variables
df$data <- substring(df$case, 0, 2)
df$vars <- str_sub(df$case, -1, -1)

# data column
df$data <- ifelse(df$data == 'Ds', 'small',
                  ifelse(df$data == 'Dm', 'medium', 
                         ifelse(df$data == 'Db', 'big', 
                                ifelse(df$data == 'Tc', 'control', 
                                       ifelse(df$data == 'Tb', 'below', 
                                              ifelse(df$data == 'Ta', 'above', 
                                                     ifelse(df$data == 'L3', '3to5', 
                                                            ifelse(df$data == 'L5', '5to6', 
                                                                   ifelse(df$data == 'L6', '6to7', 
                                                                          ifelse(df$data == 'L7', '7to9', ''))))))))))

# extra code for small_random and medium_random case studies
df$random <- substr(df$case, 0, 10)
df$data <- ifelse(df$random %in% 'Dsmall_ran', 'small_random', 
                  ifelse(df$random %in% 'Dmedium_ra', 'medium_random', df$data))
df <- dplyr::select(df, -random)

# vars columm
df$case_group <- ifelse(df$data %in% c('small', 'small_random', 'medium', 'medium_random', 'big'), 'size', 
                        ifelse(df$data %in% c('above', 'below', 'control'), 'thinning', 'records'))
df$vars <- ifelse(df$vars == 'y', 'easy', 
                  ifelse(df$vars == 'm', 'medium', 
                         ifelse(df$vars == 'd', 'hard', 'extreme')))   

# finally, I decided to not consider ANN on the analysis
df <- df[df$names_methods != 'ANN', ]

# relabel case studies: data
df$data_original <- df$data
df$data <- ifelse(df$data %in% 'small_random', 'small & random', 
                  ifelse(df$data %in% 'medium_random', 'medium & random', 
                         ifelse(df$data %in% '3to5', '3 to 5',
                                ifelse(df$data %in% '5to6', '6',
                                       ifelse(df$data %in% '6to7', '7',
                                              ifelse(df$data %in% '7to9', '7 to 9',
                                                     df$data))))))

# relabel case studies: vars
df$vars_original <- df$vars
df$vars <- ifelse(df$vars == 'easy', 'I', 
                        ifelse(df$vars == 'medium', 'II', 
                               ifelse(df$vars == 'hard', 'III', 'IV')))   

# relabel case studies: case
df$case_original <- df$case
df$case <- paste(df$data, ' - ', df$vars, sep = '')


#### Data management: time ####

# split case study in 3 variables
all_timers$data <- substring(all_timers$case, 0, 2)
all_timers$vars <- str_sub(all_timers$case, -1, -1)

# data column
all_timers$data <- ifelse(all_timers$data == 'Ds', 'small',
                          ifelse(all_timers$data == 'Dm', 'medium', 
                                 ifelse(all_timers$data == 'Db', 'big', 
                                        ifelse(all_timers$data == 'Tc', 'control', 
                                               ifelse(all_timers$data == 'Tb', 'below', 
                                                      ifelse(all_timers$data == 'Ta', 'above', 
                                                             ifelse(all_timers$data == 'L3', '3to5', 
                                                                    ifelse(all_timers$data == 'L5', '5to6', 
                                                                           ifelse(all_timers$data == 'L6', '6to7', 
                                                                                  ifelse(all_timers$data == 'L7', '7to9', ''))))))))))

# extra code for small_random and medium_random case studies
all_timers$random <- substr(all_timers$case, 0, 10)
all_timers$data <- ifelse(all_timers$random %in% 'Dsmall_ran', 'small_random', 
                    ifelse(all_timers$random %in% 'Dmedium_ra', 'medium_random', all_timers$data))
all_timers <- dplyr::select(all_timers, -random)

# vars column
all_timers$case_group <- ifelse(all_timers$data %in% c('small', 'small_random', 'medium', 'medium_random', 'big'), 'size', 
                                ifelse(all_timers$data %in% c('above', 'below', 'control'), 'thinning', 'records'))
all_timers$vars <- ifelse(all_timers$vars == 'y', 'easy', 
                          ifelse(all_timers$vars == 'm', 'medium', 
                                 ifelse(all_timers$vars == 'd', 'hard', 'extreme')))   

# filter data (just the full training process)
all_timers <- all_timers[all_timers$classifier %in% names_methods, ]

# calculate time needed per model fitted
all_timers$seconds_per_model <- ifelse(all_timers$vars == 'easy', all_timers$seconds_per_model <- all_timers$seconds/10,
                                       ifelse(all_timers$vars == 'medium', all_timers$seconds_per_model <- all_timers$seconds/30,
                                              ifelse(all_timers$vars == 'hard', all_timers$seconds_per_model <- all_timers$seconds/90,
                                                     all_timers$seconds_per_model <- all_timers$seconds/180)))
all_timers$mins_per_model <- all_timers$seconds_per_model/60

# finally, I decided to not consider ANN on the analysis
all_timers <- all_timers[all_timers$classifier != 'ANN', ]

# relabel case studies: data
all_timers$data_original <- all_timers$data
all_timers$data <- ifelse(all_timers$data %in% 'small_random', 'small & random', 
                  ifelse(all_timers$data %in% 'medium_random', 'medium & random', 
                         ifelse(all_timers$data %in% '3to5', '3 to 5',
                                ifelse(all_timers$data %in% '5to6', '6',
                                       ifelse(all_timers$data %in% '6to7', '7',
                                              ifelse(all_timers$data %in% '7to9', '7 to 9',
                                                     all_timers$data))))))

# relabel case studies: vars
all_timers$vars_original <- all_timers$vars
all_timers$vars <- ifelse(all_timers$vars == 'easy', 'I', 
                  ifelse(all_timers$vars == 'medium', 'II', 
                         ifelse(all_timers$vars == 'hard', 'III', 'IV')))   

# relabel case studies: case
all_timers$case_original <- all_timers$case
all_timers$case <- paste(all_timers$data, ' - ', all_timers$vars, sep = '')

#### Analysis of dataset size ####

# load graph functions
source('2_code/8.3_graph_functions.r')

# just size data
df_cs <- df[df$case_group == 'size', ]


#### Analysis of dataset size: graph by size groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("small", 'small & random', 'medium', "medium & random", "big") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$data <- factor(df_cs$data, levels = desired_order)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$names_methods
g_title <- 'Best model per classifier performance among dataset sizes'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$data
df_graph <- df_cs
graph_title <- 'size/classifiers_per_size'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset size: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen1', 'darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$data
g_title <- 'Best model per classifier performance among dataset sizes'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Data size'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$names_methods
df_graph <- df_cs
graph_title <- 'size/classifiers_per_size_split'

# graph results
bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset variables ####

# all data
df_cs <- df


#### Analysis of dataset variables: graph by var groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("I", "II", "III", "IV") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$vars <- factor(df_cs$vars, levels = desired_order)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$names_methods
g_title <- 'Best model per classifier performance among number of variables in the model'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$vars
df_graph <- df_cs
graph_title <- 'vars/classifiers_per_variables'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset variables: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$vars
g_title <- 'Best model per classifier performance among number of variables in the model'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Variables'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$names_methods
df_graph <- df_cs
graph_title <- 'vars/classifiers_per_variables_split'

# graph results
bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset size x variables ####

# all data
df_cs <- df[df$case_group == 'size', ]


#### Analysis of dataset variables: graph by var groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("small - I", "small - II", "small - III", "small - IV",
                   "small & random - I", "small & random - II", "small & random - III", "small & random - IV",
                   "medium - I", "medium - II", "medium - III", "medium - IV",
                   "medium & random - I", "medium & random - II", "medium & random - III", "medium & random - IV",
                   "big - I", "big - II", "big - III", "big - IV") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$case <- factor(df_cs$case, levels = desired_order)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$names_methods
g_title <- 'Best model per classifier performance among dataset size and number of variables'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$case
df_graph <- df_cs
graph_title <- 'size-vars/classifiers_per_size-vars'

# graph results
bar_by_cs_size_var(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset size: graph by classifiers ####

# new color scale
# color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen',
#                          'lightgreen', 'palegreen', 'mediumseagreen','seagreen',
#                          'olivedrab1', 'olivedrab2', 'olivedrab3', 'olivedrab4')
color_methods_cases <- c(
  "#556B2F", "#6B8E23", "#8FBC8F", "#228B22",  # darkolivegreen shades
  "#98FB98", "#90EE90", "#3CB371", "#2E8B57",  # green shades
  "#C0FF3E", "#B4EEB4", "#9BCD9B", "#698B69",  # olivedrab shades
  "#00FA9A", "#2E8B57", "#32CD32", "#008B45",  # mediumseagreen shades with different tones
  "#20B2AA", "#5F9EA0", "#008B8B", "#008080"   # teal/turquoise shades
)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$case
g_title <- 'Best model per classifier performance among dataset size and number of variables'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Case study'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$names_methods
df_graph <- df_cs
graph_title <- 'size-vars/classifiers_per_size-vars_split'

# graph results
bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)



#### Analysis of dataset thinning ####

# all data
df_cs <- df[df$case_group == 'thinning', ]


#### Analysis of dataset thinning: graph by thinning groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("control", "below", "above") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$data <- factor(df_cs$data, levels = desired_order)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$names_methods
g_title <- 'Best model per classifier performance among thinning regimes'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$data
df_graph <- df_cs
graph_title <- 'thinning/classifiers_per_thinning'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset thinning: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$data
g_title <- 'Best model per classifier performance among thinning regimes'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Thinning regime'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$names_methods
df_graph <- df_cs
graph_title <- 'thinning/classifiers_per_thinning_split'

# graph results
bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset thinning x variables ####

# all data
df_cs <- df[df$case_group == 'thinning', ]


#### Analysis of dataset thinning: graph by var groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("control - I", "control - II", "control - III", "control - IV",
                   "below - I", "below - II", "below - III", "below - IV",
                   "above - I", "above - II", "above - III", "above - IV") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$case <- factor(df_cs$case, levels = desired_order)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$names_methods
g_title <- 'Best model per classifier performance among thinning regimes and number of variables'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$case
df_graph <- df_cs
graph_title <- 'thinning-vars/classifiers_per_thinning-vars'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset thinning: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen',
                         'lightgreen', 'palegreen', 'mediumseagreen','seagreen',
                         'olivedrab1', 'olivedrab2', 'olivedrab3', 'olivedrab4')

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$case
g_title <- 'Best model per classifier performance among thinning regimes and number of variables'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Case study'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$names_methods
df_graph <- df_cs
graph_title <- 'thinning-vars/classifiers_per_thinning-vars_split'

# graph results
bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)



#### Analysis of dataset records ####

# all data
df_cs <- df[df$case_group == 'records', ]


#### Analysis of dataset records: graph by record groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("3 to 5", "6", "7", "7 to 9") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$data <- factor(df_cs$data, levels = desired_order)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$names_methods
g_title <- 'Best model per classifier performance among data records length'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$data
df_graph <- df_cs
graph_title <- 'record/classifiers_per_record'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset records: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$data
g_title <- 'Best model per classifier performance among data records length'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Records length'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$names_methods
df_graph <- df_cs
graph_title <- 'record/classifiers_per_record_split'

# graph results
bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset records x variables ####

# all data
df_cs <- df[df$case_group == 'records', ]


#### Analysis of dataset records: graph by var groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("3 to 5 - I", "3 to 5 - II", "3 to 5 - III", "3 to 5 - IV",
                   "6 - I", "6 - II", "6 - III", "6 - IV",
                   "7 - I", "7 - II", "7 - III", "7 - IV",
                   "7 to 9 - I", "7 to 9 - II", "7 to 9 - III", "7 to 9 - IV")

# reorder the levels of the 'data' variable in your dataframe
df_cs$case <- factor(df_cs$case, levels = desired_order)

# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$names_methods
g_title <- 'Best model per classifier performance among data records length and number of variables'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$case
df_graph <- df_cs
graph_title <- 'record-vars/classifiers_per_record-vars'

# graph results
bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset records: graph by classifiers ####

# new color scale
# color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen',
#                          'lightgreen', 'palegreen', 'mediumseagreen','seagreen',
#                          'olivedrab1', 'olivedrab2', 'olivedrab3', 'olivedrab4')
color_methods_cases <- c(
  "#556B2F", "#6B8E23", "#8FBC8F", "#228B22",  # darkolivegreen shades
  "#98FB98", "#90EE90", "#3CB371", "#2E8B57",  # green shades
  "#C0FF3E", "#B4EEB4", "#9BCD9B", "#698B69",  # olivedrab shades
  "#00FA9A", "#2E8B57", "#32CD32", "#008B45")  # mediumseagreen shades with different tones
  #"#20B2AA", "#5F9EA0", "#008B8B", "#008080"   # teal/turquoise shades


# graph information
x_axis <- df_cs$names_methods
y_axis <- df_cs$best_acc
fill <- df_cs$case
g_title <- 'Best model per classifier performance among data records length and number of variables'
g_x <- 'Classifier'
g_y <- 'MCC'
g_legend <- 'Case study'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$names_methods
df_graph <- df_cs
graph_title <- 'record-vars/classifiers_per_record-vars_split'

# graph results
bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of time: dataset size ####

# just size data
df_cs <- all_timers[all_timers$case_group == 'size', ]

# filter random dataset as the amount of data is the same
df_cs <- df_cs[df_cs$data %in% c('small', 'medium', 'big'), ]

#### Analysis of time by size: graph by size groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- names_methods

# reorder the levels of the 'data' variable in your dataframe
df_cs$classifier <- factor(df_cs$classifier, levels = desired_order)

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$mins
fill <- df_cs$classifier
g_title <- 'Time spent per classifier among dataset sizes'
g_x <- 'Classifier'
g_y <- 'Time (minutes)'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- factor(df_cs$data, c('small', 'small & random', 'medium', 'medium & random', 'big'))
df_graph <- df_cs
graph_title <- 'time/time-classifiers_per_size'

# graph results
time_bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset size: graph by classifiers ####

# new color scale
# color_methods_cases <- c('darkolivegreen1', 'darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')
color_methods_cases <- c('darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$mins
fill <- factor(df_cs$data, c('small', 'small & random', 'medium', 'medium & random', 'big'))
g_title <- 'Time spent per classifier among dataset sizes'
g_x <- 'Classifier'
g_y <- 'Time (minutes)'
g_legend <- 'Dataset size'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$classifier
df_graph <- df_cs
graph_title <- 'time/time-classifiers_per_size_split'

# graph results
time_bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)



#### Analysis of time: variables groups ####

# just size data
df_cs <- all_timers

# filter random dataset as the amount of data is the same
df_cs <- df_cs[df_cs$data %in% c('small', 'medium', 'big'), ]


#### Analysis of time by variables: graph by variables groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- names_methods

# reorder the levels of the 'data' variable in your dataframe
df_cs$classifier <- factor(df_cs$classifier, levels = desired_order)

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$mins
fill <- df_cs$classifier
g_title <- 'Time spent per classifier among variables groups'
g_x <- 'Classifier'
g_y <- 'Time (minutes)'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- factor(df_cs$vars, c("I", "II", "III", "IV"))
df_graph <- df_cs
graph_title <- 'time/time-classifiers_per_vars'

# graph results
time_bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of time: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$mins
fill <- factor(df_cs$vars, c("I", "II", "III", "IV"))
g_title <- 'Time spent per classifier among variable groups'
g_x <- 'Classifier'
g_y <- 'Time (minutes)'
g_legend <- 'Variables'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$classifier
df_graph <- df_cs
graph_title <- 'time/time-classifiers_per_vars_split'

# graph results
time_bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of time size x variables ####

# all data
df_cs <- all_timers[all_timers$case_group == 'size', ]

# filter random dataset as the amount of data is the same
df_cs <- df_cs[df_cs$data %in% c('small', 'medium', 'big'), ]


#### Analysis of time: graph by size x var groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("small - I", "small - II", "small - III", "small - IV",
                   "medium - I", "medium - II", "medium - III", "medium - IV",
                   "big - I", "big - II", "big - III", "big - IV") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$case <- factor(df_cs$case, levels = desired_order)

# graph information
x_axis <- factor(df_cs$classifier, levels = names_methods)
y_axis <- df_cs$mins
fill <- factor(df_cs$classifier, levels = names_methods)
g_title <- 'Time spent per classifier among dataset size and number of variables'
g_x <- 'Classifier'
g_y <- 'Time (minutes)'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$case
df_graph <- df_cs
graph_title <- 'time/time-classifiers_per_size-vars'

# graph results
time_bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset size: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen',
                         'lightgreen', 'palegreen', 'mediumseagreen','seagreen',
                         'olivedrab1', 'olivedrab2', 'olivedrab3', 'olivedrab4')

# graph information
x_axis <- df_cs$case
y_axis <- df_cs$mins
fill <- df_cs$case
g_title <- 'Time spent per classifier among dataset size and number of variables'
g_x <- 'Classifier'
g_y <- 'Time (minutes)'
g_legend <- 'Case studies'
color_groups <- color_methods_cases
df_cs$facet <- factor(df_cs$classifier, levels = names_methods)
df_graph <- df_cs
graph_title <- 'time/time-classifiers_per_size-vars_split'

# graph results
time_bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Time is studies again just for 1 model ####


#### Analysis of time: dataset size ####

# just size data
df_cs <- all_timers[all_timers$case_group == 'size', ]

# filter random dataset as the amount of data is the same
df_cs <- df_cs[df_cs$data %in% c('small', 'medium', 'big'), ]


#### Analysis of time by size: graph by size groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- names_methods

# reorder the levels of the 'data' variable in your dataframe
df_cs$classifier <- factor(df_cs$classifier, levels = desired_order)

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$seconds_per_model
fill <- df_cs$classifier
g_title <- 'Time spent per classifier among dataset sizes'
g_x <- 'Classifier'
g_y <- 'Time (seconds)'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- factor(df_cs$data, c('small', 'medium', 'big'))
df_graph <- df_cs
graph_title <- 'time-1_model/time-classifiers_per_size'

# graph results
time_bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset size: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$seconds_per_model
fill <- factor(df_cs$data, c('small', 'medium', 'big'))
g_title <- 'Time spent per classifier among dataset sizes'
g_x <- 'Classifier'
g_y <- 'Time (seconds)'
g_legend <- 'Dataset size'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$classifier
df_graph <- df_cs
graph_title <- 'time-1_model/time-classifiers_per_size_split'

# graph results
time_bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)



#### Analysis of time: variables groups ####

# just size data
df_cs <- all_timers

# filter random dataset as the amount of data is the same
df_cs <- df_cs[df_cs$data %in% c('small', 'medium', 'big'), ]


#### Analysis of time by variables: graph by variables groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- names_methods

# reorder the levels of the 'data' variable in your dataframe
df_cs$classifier <- factor(df_cs$classifier, levels = desired_order)

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$seconds_per_model
fill <- df_cs$classifier
g_title <- 'Time spent per classifier among variables groups'
g_x <- 'Classifier'
g_y <- 'Time (seconds)'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- factor(df_cs$vars, c("I", "II", "III", "IV"))
df_graph <- df_cs
graph_title <- 'time-1_model/time-classifiers_per_vars'

# graph results
time_bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of time: graph by classifiers ####

# new color scale
color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen')

# graph information
x_axis <- df_cs$classifier
y_axis <- df_cs$seconds_per_model
fill <- factor(df_cs$vars, c("I", "II", "III", "IV"))
g_title <- 'Time spent per classifier among variable groups'
g_x <- 'Classifier'
g_y <- 'Time (seconds)'
g_legend <- 'Variables'
color_groups <- color_methods_cases
df_cs$facet <- df_cs$classifier
df_graph <- df_cs
graph_title <- 'time-1_model/time-classifiers_per_vars_split'

# graph results
time_bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)



#### Analysis of time size x variables ####

# all data
df_cs <- all_timers[all_timers$case_group == 'size', ]

# filter random dataset as the amount of data is the same
df_cs <- df_cs[df_cs$data %in% c('small', 'medium', 'big'), ]


#### Analysis of time: graph by size x var groups ####

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c("small - I", "small - II", "small - III", "small - IV",
                   "small & random - I", "small & random - II", "small & random - III", "small & random - IV",
                   "medium - I", "medium - II", "medium - III", "medium - IV",
                   "medium & random - I", "medium & random - II", "medium & random - III", "medium & random - IV",
                   "big - I", "big - II", "big - III", "big - IV") 

# reorder the levels of the 'data' variable in your dataframe
df_cs$case <- factor(df_cs$case, levels = desired_order)

# graph information
x_axis <- factor(df_cs$classifier, levels = names_methods)
y_axis <- df_cs$seconds_per_model
fill <- factor(df_cs$classifier, levels = names_methods)
g_title <- 'Time spent per classifier among dataset size and number of variables'
g_x <- 'Classifier'
g_y <- 'Time (seconds)'
g_legend <- 'Classifiers'
color_groups <- color_methods
df_cs$facet <- df_cs$case
df_graph <- df_cs
graph_title <- 'time-1_model/time-classifiers_per_size-vars'

# graph results
time_bar_by_cs(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)


#### Analysis of dataset size: graph by classifiers ####

# new color scale
# color_methods_cases <- c('darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkolivegreen',
#                          'lightgreen', 'palegreen', 'mediumseagreen','seagreen',
#                          'olivedrab1', 'olivedrab2', 'olivedrab3', 'olivedrab4')
color_methods_cases <- c(
  "#556B2F", "#6B8E23", "#8FBC8F", "#228B22",  # darkolivegreen shades
  "#98FB98", "#90EE90", "#3CB371", "#2E8B57",  # green shades
  "#C0FF3E", "#B4EEB4", "#9BCD9B", "#698B69",  # olivedrab shades
  "#00FA9A", "#2E8B57", "#32CD32", "#008B45",  # mediumseagreen shades with different tones
  "#20B2AA", "#5F9EA0", "#008B8B", "#008080"   # teal/turquoise shades
)

# graph information
x_axis <- df_cs$case
y_axis <- df_cs$seconds_per_model
fill <- df_cs$case
g_title <- 'Time spent per classifier among dataset size and number of variables'
g_x <- 'Classifier'
g_y <- 'Time (seconds)'
g_legend <- 'Case studies'
color_groups <- color_methods_cases
df_cs$facet <- factor(df_cs$classifier, levels = names_methods)
df_graph <- df_cs
graph_title <- 'time-1_model/time-classifiers_per_size-vars_split'

# graph results
time_bar_by_classifiers(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title)

