#------------------------------------------------------------------------------------------#
####                              Case studies: analysis                                ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 27/10/2023                              #
#                              Last modification: 22/01/2025                               #
#------------------------------------------------------------------------------------------#


#### Basic steps ####

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(factoextra)

setwd('/media/aitor/WDE/PhD_UVa/1_Topics/2_Vitality/')


#### Load general information ####

# load the variables groups from the previous code
load('1_data/3_final/6_final_results/best_models.RData')

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
# all_timers <- all_timers[all_timers$classifier %in% names_methods, ]

# calculate time needed per model fitted
# all_timers$seconds_per_model <- ifelse(all_timers$vars == 'easy', all_timers$seconds_per_model <- all_timers$seconds/10,
#                                        ifelse(all_timers$vars == 'medium', all_timers$seconds_per_model <- all_timers$seconds/30,
#                                               ifelse(all_timers$vars == 'hard', all_timers$seconds_per_model <- all_timers$seconds/90,
#                                                      all_timers$seconds_per_model <- all_timers$seconds/180)))
# all_timers$mins_per_model <- all_timers$seconds_per_model/60

# finally, I decided to not consider ANN on the analysis
# all_timers <- all_timers[all_timers$classifier != 'ANN', ]

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


#### PERFORMANCE GRAPHS ####

#### number of variables ####

df_vars <- df

# just big dataset
df_vars <- df_vars[df_vars$data == 'big', ]

# graph
g_var <- ggplot(df_vars, aes(x = vars, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1) +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithm performance by number of variables") +
  xlab(NULL)  # Removes the x-axis title

g_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/vars/lines_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graph
gdash_var <- ggplot(df_vars, aes(x = vars, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithm performance by number of variables") +
  xlab(NULL)  # Removes the x-axis title

gdash_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/vars/lines_dash_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graph points
gpoint_var <- ggplot(df_vars, aes(x = vars, y = best_acc, color = names_methods)) +
  geom_point(size = 7) +
  # geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithm performance by number of variables") +
  xlab(NULL)  # Removes the x-axis title

gpoint_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/vars/point_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# Bar graph
gbar_var <- ggplot(df_vars, aes(x = vars, y = best_acc, fill = names_methods)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Algorithm", y = 'MCC') +
  scale_fill_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  ggtitle("Algorithm performance by number of variables") +
  xlab(NULL)  # Removes the x-axis title

gbar_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/vars/bars_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### dataset size ####
df_size <- df

# just vars III
df_size <- df_size[df_size$case_group == 'size', ]
df_size <- df_size[df_size$vars == 'III', ]

# just random datasets
df_size <- df_size[df_size$data %in% c('small & random', 'medium & random', 'big'), ]

# rename classes
df_size$data <- ifelse(df_size$data == 'small & random', 'small', 
                       ifelse(df_size$data == 'medium & random', 'medium', 'big'))

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c('small', 'medium', 'big') 

# reorder the levels of the 'data' variable in your dataframe
df_size$data <- factor(df_size$data, levels = desired_order)

# graph
g_size <- ggplot(df_size, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1) +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
    ) +
  ggtitle("Algorithm performance by dataset size") +
  xlab(NULL)  # Removes the x-axis title

g_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/size/lines_size.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graph
gdash_size <- ggplot(df_size, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithm performance by dataset size") +
  xlab(NULL)  # Removes the x-axis title

gdash_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/size/lines_dash_size.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graph point
gpoint_size <- ggplot(df_size, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 7) +
  # geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithm performance by dataset size") +
  xlab(NULL)  # Removes the x-axis title

gpoint_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/size/point_size.png", units = 'mm', dpi = 600, width = 450, height = 300)

# Bar graph
gbar_size <- ggplot(df_size, aes(x = data, y = best_acc, fill = names_methods)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Algorithm", y = 'MCC') +
  scale_fill_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  ggtitle("Algorithm performance by dataset size") +
  xlab(NULL)  # Removes the x-axis title

gbar_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/size/bars_size.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### dataset size and variables ####
df2 <- df[df$case_group == 'size', ]

# select data and manage it
df2 <- df2[df2$data %in% c('small & random', 'medium & random', 'big'), ]
desired_order <- c('small & random', 'medium & random', 'big') 
df2$data <- factor(df2$data, levels = desired_order)
df2$data <- ifelse(df2$data == 'small & random', 'small', 
                   ifelse(df2$data == 'medium & random', 'medium', 'big'))

# graph
g_sizevar <- ggplot(df2, aes(x = vars, y = names_methods, fill = best_acc)) +
  geom_tile() +
  scale_fill_gradient('MCC ', low = "white", high = "darkgreen") +
  xlab('') +
  ylab('') +
  ggtitle('Algorithm performance by dataset size and number of variables') +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    strip.text.x = element_text(size = 20)
  ) +
  facet_wrap(~factor(data, levels = c('small', 'medium', 'big')), ncol = 3)

g_sizevar

ggsave("2_scripts/4_figures/9.4_best_model_metrics/size-vars/heatmap_size-vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# grouped graphs
g_var <- g_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_size <- g_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(g_size, g_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", hjust = 0, labels = c('', 'C'), font.label = list(size = 20), widths = c(1, 1.2))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_size-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - dashed lines
gdash_var <- gdash_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
gdash_size <- gdash_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(gdash_size, gdash_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", hjust = 0, labels = c('', 'C'), font.label = list(size = 20), widths = c(1, 1.2))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_dashed_size-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - points
gpoint_var <- gpoint_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
gpoint_size <- gpoint_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(gpoint_size, gpoint_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", hjust = 0, labels = c('', 'C'), font.label = list(size = 20), widths = c(1, 1.2))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_point_size-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - bars
gbar_var <- gbar_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
gbar_size <- gbar_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(gbar_size, gbar_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", hjust = 0, labels = c('', 'C'), font.label = list(size = 20), widths = c(1, 1.2))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_bar_size-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)



#### thinning grades ####
df_th <- df

# just vars III
df_th <- df_th[df_th$case_group == 'thinning', ]
df_th <- df_th[df_th$vars == 'III', ]

# reorder variables
desired_order <- c('control', 'above', 'below')
df_th$data <- factor(df_th$data, levels = desired_order)

# graph
g_th <- ggplot(df_th, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1) +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
    ) +
  ggtitle("Algorithm performance by thinning grades") +
  xlab(NULL)  # Removes the x-axis title

g_th

ggsave("2_scripts/4_figures/9.4_best_model_metrics/thinning/lines_thinning.png", units = 'mm', dpi = 600, width = 450, height = 300)

# dash graph
gdash_th <- ggplot(df_th, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithm performance by thinning grades") +
  xlab(NULL)  # Removes the x-axis title

gdash_th

ggsave("2_scripts/4_figures/9.4_best_model_metrics/thinning/lines_dash_thinning.png", units = 'mm', dpi = 600, width = 450, height = 300)

# point graph
gpoint_th <- ggplot(df_th, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 7) +
  # geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithm performance by thinning grades") +
  xlab(NULL)  # Removes the x-axis title

gpoint_th

ggsave("2_scripts/4_figures/9.4_best_model_metrics/thinning/point_thinning.png", units = 'mm', dpi = 600, width = 450, height = 300)

# Bar graph
gbar_th <- ggplot(df_th, aes(x = data, y = best_acc, fill = names_methods)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Algorithm", y = 'MCC') +
  scale_fill_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  ggtitle("Algorithm performance by thinning grades") +
  xlab(NULL)  # Removes the x-axis title

gbar_th

ggsave("2_scripts/4_figures/9.4_best_model_metrics/thinning/bars_thinning.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### thinning grades and variables ####
df3 <- df[df$case_group == 'thinning', ]

# graph
g_thvar <- ggplot(df3, aes(x = vars, y = names_methods, fill = best_acc)) +
  geom_tile() +
  scale_fill_gradient('MCC ', low = "white", high = "darkgreen") +
  xlab('') +
  ylab('') +
  ggtitle('Algorithm performance by thinning grades and amount of variables') +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    strip.text.x = element_text(size = 20)
    ) +
  facet_wrap(~factor(data, levels = c('control', 'above', 'below')), ncol = 3)

g_thvar

ggsave("2_scripts/4_figures/9.4_best_model_metrics/thinning-vars/heatmap_thinning-vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# grouped graphs
g_th <- g_th  + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_thvar <- g_thvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(g_th, g_thvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_thinning-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - dashed
gdash_th <- gdash_th  + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_thvar <- g_thvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(gdash_th, g_thvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_dash_thinning-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - point
gpoint_th <- gpoint_th + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_thvar <- g_thvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(gpoint_th, g_thvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_point_thinning-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)


# grouped graphs - bars
gbar_th <- gbar_th + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_thvar <- g_thvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(gbar_th, g_thvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_bar_thinning-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)


#### dataset records length ####
df_rec <- df

# just vars III
df_rec <- df_rec[df_rec$case_group == 'records', ]
df_rec <- df_rec[df_rec$vars == 'III', ]
df_rec$data <- ifelse(df_rec$data == '7 to 9', '8 to 9', df_rec$data)

# graph
g_rec <- ggplot(df_rec, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1) +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
    ) +
  ggtitle("Algorithms performance by inventory record length") +
  xlab(NULL)  # Removes the x-axis title

g_rec

ggsave("2_scripts/4_figures/9.4_best_model_metrics/record/lines_records.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graph dashed
gdash_rec <- ggplot(df_rec, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 3) +
  geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithms performance by inventory record length") +
  xlab(NULL)  # Removes the x-axis title

gdash_rec

ggsave("2_scripts/4_figures/9.4_best_model_metrics/record/lines_dash_records.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graph point
gpoint_rec <- ggplot(df_rec, aes(x = data, y = best_acc, color = names_methods)) +
  geom_point(size = 7) +
  # geom_line(aes(group = names_methods), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'MCC') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Algorithms performance by inventory record length") +
  xlab(NULL)  # Removes the x-axis title

gpoint_rec

ggsave("2_scripts/4_figures/9.4_best_model_metrics/record/point_records.png", units = 'mm', dpi = 600, width = 450, height = 300)

# Bar graph
gbar_rec <- ggplot(df_rec, aes(x = data, y = best_acc, fill = names_methods)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(fill = "Algorithm", y = 'MCC') +
  scale_fill_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  ggtitle("Algorithms performance by inventory record length") +
  xlab(NULL)  # Removes the x-axis title

gbar_rec

ggsave("2_scripts/4_figures/9.4_best_model_metrics/record/bars_records.png", units = 'mm', dpi = 600, width = 450, height = 300)

#### record lenght and variables ####
df4 <- df[df$case_group == 'records', ]

g_recvar <- ggplot(df4, aes(x = vars, y = names_methods, fill = best_acc)) +
  geom_tile() +
  scale_fill_gradient('MCC ', low = "white", high = "darkgreen") +
  xlab('') +
  ylab('') +
  ggtitle('Algorithms performance by inventory record length and number of variables') +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    strip.text.x = element_text(size = 20)
  ) +
  facet_wrap(~data, ncol = 2)

g_recvar

ggsave("2_scripts/4_figures/9.4_best_model_metrics/record-vars/heatmap_record-vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# grouped graphs
g_rec <- g_rec + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_recvar <- g_recvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(g_rec, g_recvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_record-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - dashed
gdash_rec <- gdash_rec + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_recvar <- g_recvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(gdash_rec, g_recvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_dash_record-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - dashed
gpoint_rec <- gpoint_rec + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_recvar <- g_recvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(gpoint_rec, g_recvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_point_record-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graphs - bars
gbar_rec <- gbar_rec + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_recvar <- g_recvar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
ggarrange(gbar_rec, g_recvar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.2), hjust = 0, labels = c('A', 'B'), font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/grouped_bar_record-vars.png", units = 'mm', dpi = 600, width = 600, height = 300)


#### size x thinning ####
new_df <- df
new_df <- new_df[new_df$case_group %in% c('size', 'thinning'), ]

# just vars III
new_df <- new_df[new_df$vars == 'III', ]
new_df <- new_df[new_df$data %in% c('small & random', 'medium & random', 'big', 'control', 'above', 'below'), ]
new_df <- spread(new_df, key = data, value = best_acc)

# summarize data
new_df <- plyr::ddply(new_df, 'names_methods', summarise,
                       `big` = mean(`big`, na.rm = TRUE),
                       `medium` = mean(`medium & random`, na.rm = TRUE),
                       `small` = mean(`small & random`, na.rm = TRUE),
                       `control` = mean(`control`, na.rm = TRUE),
                       `above` = mean(`above`, na.rm = TRUE),
                       `below` = mean(`below`, na.rm = TRUE)
)

# get labels to rownames
labels <- new_df[, 1:1]
new_df <- new_df[, -1]
rownames(new_df) <- labels

# graph function
graph <- function(df, x, y, x_lab, y_lab, color_methods){
  
  color_mapping <- setNames(color_methods, rownames(df))
  
  ggplot(df, aes(x = x, y = y, color = rownames(df))) +
    geom_point(size = 3) +
    xlim(0.35, 0.8) + ylim(0.35, 0.8) +
    geom_text(aes(label = rownames(df)), size = 3, vjust = 1.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_color_manual(values = color_mapping) # colors
  
}

# make graphs
g1 <- graph(new_df, new_df$small, new_df$control, 'small', 'control', color_methods)
g2 <- graph(new_df, new_df$medium, new_df$control, 'medium', 'control', color_methods)
g3 <- graph(new_df, new_df$big, new_df$control, 'big', 'control', color_methods)
g4 <- graph(new_df, new_df$small, new_df$above, 'small', 'above', color_methods)
g5 <- graph(new_df, new_df$medium, new_df$above, 'medium', 'above', color_methods)
g6 <- graph(new_df, new_df$big, new_df$above, 'big', 'above', color_methods)
g7 <- graph(new_df, new_df$small, new_df$below, 'small', 'below', color_methods)
g8 <- graph(new_df, new_df$medium, new_df$below, 'medium', 'below', color_methods)
g9 <- graph(new_df, new_df$big, new_df$below, 'big', 'below', color_methods)

# compile graphs in one
ggarrange(g1, g2, g3, 
          g4, g5, g6,
          g7, g8, g9, 
          ncol = 3, nrow = 3, 
          common.legend = TRUE, legend = "none",  # "bottom"
          labels = c('C1', 'C2', 'C3', 'A1', 'A2', 'A3', 'B1', 'B2', 'B3'), hjust = 0, font.label = list(size = 20))

ggsave("2_scripts/4_figures/9.4_best_model_metrics/size-thinning/size_thinning.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### size x records ####
new_df <- df
new_df <- new_df[new_df$case_group %in% c('size', 'records'), ]

# just vars III
new_df <- new_df[new_df$vars == 'III', ]
new_df <- new_df[new_df$data %in% c('small & random', 'medium & random', 'big', '3 to 5', '6', '7', '7 to 9'), ]
new_df <- spread(new_df, key = data, value = best_acc)

# summarize data
new_df <- plyr::ddply(new_df, 'names_methods', summarise,
                      `big` = mean(`big`, na.rm = TRUE),
                      `medium` = mean(`medium & random`, na.rm = TRUE),
                      `small` = mean(`small & random`, na.rm = TRUE),
                      `3 to 5` = mean(`3 to 5`, na.rm = TRUE),
                      `6` = mean(`6`, na.rm = TRUE),
                      `7` = mean(`7`, na.rm = TRUE),
                      `7 to 9` = mean(`7 to 9`, na.rm = TRUE)
)

# get labels to rownames
labels <- new_df[, 1:1]
new_df <- new_df[, -1]
rownames(new_df) <- labels

# graph function
graph <- function(df, x, y, x_lab, y_lab, color_methods){
  
  color_mapping <- setNames(color_methods, rownames(df))
  
  ggplot(df, aes(x = x, y = y, color = rownames(df))) +
    geom_point(size = 3) +
    xlim(0.35, 0.8) + ylim(0.35, 0.8) +
    geom_text(aes(label = rownames(df)), size = 3, vjust = 1.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_color_manual(values = color_mapping) # colors
  
}

# make graphs
g1 <- graph(new_df, new_df$small, new_df$`3 to 5`, 'small', '3 to 5', color_methods)
g2 <- graph(new_df, new_df$medium, new_df$`3 to 5`, 'medium', '3 to 5', color_methods)
g3 <- graph(new_df, new_df$big, new_df$`3 to 5`, 'big', '3 to 5', color_methods)
g4 <- graph(new_df, new_df$small, new_df$`6`, 'small', '6', color_methods)
g5 <- graph(new_df, new_df$medium, new_df$`6`, 'medium', '6', color_methods)
g6 <- graph(new_df, new_df$big, new_df$`6`, 'big', '6', color_methods)
g7 <- graph(new_df, new_df$small, new_df$`7`, 'small', '7', color_methods)
g8 <- graph(new_df, new_df$medium, new_df$`7`, 'medium', '7', color_methods)
g9 <- graph(new_df, new_df$big, new_df$`7`, 'big', '7', color_methods)
g10 <- graph(new_df, new_df$small, new_df$`7 to 9`, 'small', '7 to 9', color_methods)
g11 <- graph(new_df, new_df$medium, new_df$`7 to 9`, 'medium', '7 to 9', color_methods)
g12 <- graph(new_df, new_df$big, new_df$`7 to 9`, 'big', '7 to 9', color_methods)

# compile graphs in one
ggarrange(g1, g2, g3, 
          g4, g5, g6,
          g7, g8, g9, 
          g10, g11, g12,
          ncol = 3, nrow = 4, 
          common.legend = TRUE, legend = "none",  # "bottom"
          labels = c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3', 'D1', 'D2', 'D3'), hjust = 0, font.label = list(size = 20))

ggsave("2_scripts/4_figures/9.4_best_model_metrics/size-records/size_records.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### TIME GRAPHS ####

# remove from environment except all_timers
rm(list = setdiff(ls(), c("all_timers", "color_methods", "names_methods")))

# reorder dataset
all_timers$classifier <- factor(all_timers$classifier, levels = names_methods)

# just random datasets
all_timers <- all_timers[all_timers$data %in% c('small & random', 'medium & random', 'big'), ]

# rename classes
all_timers$data <- ifelse(all_timers$data == 'small & random', 'small', 
                       ifelse(all_timers$data == 'medium & random', 'medium', 'big'))

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c('small', 'medium', 'big') 

# reorder the levels of the 'data' variable in your dataframe
all_timers$data <- factor(all_timers$data, levels = desired_order)


#### number of variables ####

df <- all_timers

# just big dataset
df <- df[df$data == 'big', ]

# new column with size and number of variables
df$size_vars <- paste(df$data, '-', df$n_vars, 'vars', sep = ' ')

# delete grouped classifiers processes and provide new labels
df <- df[is.na(df$classifier), ]
df$classifier <- ifelse(grepl('lr', df$msg), 'LR', 
                        ifelse(grepl('rf', df$msg), 'RF', 
                               ifelse(grepl('svm', df$msg), 'SVM', 
                                      ifelse(grepl('dt', df$msg), 'DT', 
                                             ifelse(grepl('nb', df$msg), 'NB', 
                                                    ifelse(grepl('knn', df$msg), 'KNN', NA))))))

# summarize by size_vars and classifier
df <- df %>% 
  group_by(data, n_vars, size_vars, classifier) %>% 
  summarise(mins_per_model = mean(mins, na.rm = TRUE)) %>% 
  ungroup()

# reorder dataset
df$classifier <- factor(df$classifier, levels = names_methods)

# graphs vars time 
# g_var <- ggplot(df, aes(x = factor(size_vars, 
#                                    levels = c('big - 4 vars', 'big - 5 vars', 'big - 6 vars', 'big - 7 vars', 
#                                               'big - 8 vars', 'big - 9 vars', 'big - 10 vars')), 
#                         y = mins_per_model, color = classifier)) +
g_var <- ggplot(df, aes(x = factor(n_vars, levels = c('4', '5', '6', '7', '8', '9', '10')), 
                        y = mins_per_model, color = classifier)) +
  geom_point(size = 3) +
  geom_line(aes(group = classifier), linewidth = 1) +
  labs(color = "Algorithm", y = 'Time (min)') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent into train a single model by number of variables") +
  xlab(NULL)  # Removes the x-axis title

g_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/lines_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graphs vars time - dashed
gdash_var <- ggplot(df, aes(x = factor(n_vars, levels = c('4', '5', '6', '7', '8', '9', '10')), 
                        y = mins_per_model, color = classifier)) +
  geom_point(size = 3) +
  geom_line(aes(group = classifier), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'Time (min)') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent into train a single model by number of variables") +
  xlab(NULL)  # Removes the x-axis title

gdash_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/lines_dash_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graphs vars time - point
gpoint_var <- ggplot(df, aes(x = factor(n_vars, levels = c('4', '5', '6', '7', '8', '9', '10')), 
                            y = mins_per_model, color = classifier)) +
  geom_point(size = 7) +
  # geom_line(aes(group = classifier), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'Time (min)') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent into train a single model by number of variables") +
  xlab(NULL)  # Removes the x-axis title

gpoint_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/point_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graphs vars time - bars
gbar_var <- ggplot(df, aes(x = factor(n_vars, levels = c('4', '5', '6', '7', '8', '9', '10')), 
                        y = mins_per_model, fill = classifier)) +  # Use fill instead of color for bars
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Create bar plot
  labs(fill = "Algorithm", y = 'Time (min)') +  # Use labs(fill = ...) for legend title
  scale_fill_manual(values = color_methods) +  # Set colors manually
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent to train a single model by number of variables") +
  xlab(NULL)  # Removes the x-axis title


gbar_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/bars_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)



#### size ####

df_size <- all_timers

# just vars III
df_size <- df_size[df_size$case_group == 'size', ]
# df_size <- df_size[df_size$vars == 'III', ]

# # just random datasets
# df_size <- df_size[df_size$data %in% c('small & random', 'medium & random', 'big'), ]
# 
# # rename classes
# df_size$data <- ifelse(df_size$data == 'small & random', 'small', 
#                        ifelse(df_size$data == 'medium & random', 'medium', 'big'))

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c('small', 'medium', 'big') 

# delete grouped classifiers processes and provide new labels
df_size <- df_size[is.na(df_size$classifier), ]
df_size$classifier <- ifelse(grepl('lr', df_size$msg), 'LR', 
                        ifelse(grepl('rf', df_size$msg), 'RF', 
                               ifelse(grepl('svm', df_size$msg), 'SVM', 
                                      ifelse(grepl('dt', df_size$msg), 'DT', 
                                             ifelse(grepl('nb', df_size$msg), 'NB', 
                                                    ifelse(grepl('knn', df_size$msg), 'KNN', NA))))))

# summarize by size_vars and classifier
df_size <- df_size %>% 
  group_by(data, classifier) %>% 
  summarise(mins_per_model = mean(mins, na.rm = TRUE)) %>% 
  ungroup()

# reorder dataset
df_size$classifier <- factor(df_size$classifier, levels = names_methods)

# reorder the levels of the 'data' variable in your dataframe
#df_size$data <- factor(df_size$data, levels = desired_order)

# graphs vars time 
g_size <- ggplot(df_size, aes(x = data, y = mins_per_model, color = classifier)) +
  geom_point(size = 3) +
  geom_line(aes(group = classifier), linewidth = 1) +
  labs(color = "Algorithm", y = 'Time (min)') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent into train a single model by amount of data") +
  xlab(NULL)  # Removes the x-axis title

g_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/lines_size.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graphs vars time - dashed
gdash_size <- ggplot(df_size, aes(x = data, y = mins_per_model, color = classifier)) +
  geom_point(size = 3) +
  geom_line(aes(group = classifier), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'Time (min)') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent into train a single model by amount of data") +
  xlab(NULL)  # Removes the x-axis title

gdash_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/lines_dash_size.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graphs vars time - point
gpoint_size <- ggplot(df_size, aes(x = data, y = mins_per_model, color = classifier)) +
  geom_point(size = 7) +
  # geom_line(aes(group = classifier), linewidth = 1, linetype = 'dashed') +
  labs(color = "Algorithm", y = 'Time (min)') +
  scale_color_manual(values = color_methods) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent into train a single model by amount of data") +
  xlab(NULL)  # Removes the x-axis title

gpoint_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/point_size.png", units = 'mm', dpi = 600, width = 450, height = 300)

# graphs vars time - bars
gbar_size <- ggplot(df_size, aes(x = data, y = mins_per_model, fill = classifier)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Create bar plot
  labs(fill = "Algorithm", y = 'Time (min)') +  # Use labs(fill = ...) for legend title
  scale_fill_manual(values = color_methods) +  # Set colors manually
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20, hjust = 0.5)  
  ) +
  ggtitle("Averaged computation time spent to train a single model by amount of data") +
  xlab(NULL)  # Removes the x-axis title

gbar_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/bars_size.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### number of variables and size ####

df <- all_timers

# just vars III
# df <- df[df$case_group == 'size', ]

# # just random datasets
# df <- df[df$data %in% c('small & random', 'medium & random', 'big'), ]
# 
# # rename classes
# df$data <- ifelse(df$data == 'small & random', 'small', 
#                        ifelse(df$data == 'medium & random', 'medium', 'big'))

# reorder the levels of the 'data' variable based on your preferred order
desired_order <- c('small', 'medium', 'big') 

# new column with size and number of variables
df$size_vars <- paste(df$data, '-', df$n_vars, 'vars', sep = ' ')

# delete grouped classifiers processes and provide new labels
df <- df[is.na(df$classifier), ]
df$classifier <- ifelse(grepl('lr', df$msg), 'LR', 
                        ifelse(grepl('rf', df$msg), 'RF', 
                               ifelse(grepl('svm', df$msg), 'SVM', 
                                      ifelse(grepl('dt', df$msg), 'DT', 
                                             ifelse(grepl('nb', df$msg), 'NB', 
                                                    ifelse(grepl('knn', df$msg), 'KNN', NA))))))

# summarize by size_vars and classifier
df <- df %>% 
  group_by(data, n_vars, size_vars, classifier) %>% 
  summarise(mins_per_model = mean(mins, na.rm = TRUE)) %>% 
  ungroup()

# reorder dataset
df$classifier <- factor(df$classifier, levels = names_methods)

# reorder the levels of the 'data' variable in your dataframe
df$data <- factor(df$data, levels = desired_order)

# graph
g_sizevar <- ggplot(df, aes(x = factor(n_vars, levels = c('4', '5', '6', '7', '8', '9', '10')), 
                            y = classifier, fill = mins_per_model)) +
  geom_tile() +
  scale_fill_gradient('Time (min) ', low = "white", high = "darkgreen") +
  xlab('') +
  ylab('') +
  ggtitle('Averaged computation time spent into train a single model by amount of data and number of variables') +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    strip.text.x = element_text(size = 20)
  ) +
  facet_wrap(~factor(data, levels = c('small', 'medium', 'big')), ncol = 3)

g_sizevar

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time/heatmap_size-var.png", units = 'mm', dpi = 600, width = 450, height = 300)

# grouped graph
g_size <- g_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_var <- g_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(g_size, g_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c('A', 'B'), hjust = 0, font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.5), labels = c('', 'C'), hjust = 0, font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/time.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graph - dashed
gdash_size <- gdash_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
gdash_var <- gdash_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(gdash_size, gdash_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c('A', 'B'), hjust = 0, font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.5), labels = c('', 'C'), hjust = 0, font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/time_dashed.png", units = 'mm', dpi = 600, width = 600, height = 300)

# grouped graph - point
gpoint_size <- gpoint_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
gpoint_var <- gpoint_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(gpoint_size, gpoint_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c('A', 'B'), hjust = 0, font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.5), labels = c('', 'C'), hjust = 0, font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/time_point.png", units = 'mm', dpi = 600, width = 600, height = 300)


# grouped graph - bars
gbar_size <- gbar_size + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
gbar_var <- gbar_var + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 16))
g_sizevar <- g_sizevar + theme(plot.title = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), legend.title = element_text(size = 16), legend.text = element_text(size = 10))
g1 <- ggarrange(gbar_size, gbar_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c('A', 'B'), hjust = 0, font.label = list(size = 20))
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.5), labels = c('', 'C'), hjust = 0, font.label = list(size = 20))
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/bars_time.png", units = 'mm', dpi = 600, width = 600, height = 300)
