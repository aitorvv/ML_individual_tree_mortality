#------------------------------------------------------------------------------------------#
####                         Time spent into training - graphs                                
#                                                                                          #
#                            Aitor Vázquez Veloso, 18/01/2024                              #
#                              Last modification: 21/01/2025                               #
#------------------------------------------------------------------------------------------#


# libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)

# working directory
setwd('/media/aitor/WDE/PhD_UVa/1_Topics/2_Vitality/')

# load time results
load('1_data/3_final/6_final_results/timer/timer_comparison.RData')

# calculate time variables
my_timer$seconds <- my_timer$toc - my_timer$tic
my_timer$mins <- my_timer$seconds / 60

# calculate extra variables
my_timer$classifier <- substring(my_timer$msg, 1, 3)
my_timer$classifier <- ifelse(grepl('_', my_timer$classifier), gsub("_","", my_timer$classifier), my_timer$classifier)

my_timer$n_vars <- substring(my_timer$msg, 4, 5)
my_timer$n_vars <- ifelse(grepl('_', my_timer$n_vars), gsub("_","", my_timer$n_vars), my_timer$n_vars)
my_timer$n_vars <- ifelse(my_timer$n_vars == '1', '10', my_timer$n_vars)

my_timer$size <- substring(my_timer$msg, 6, 15)
my_timer$size <- ifelse(grepl('_', my_timer$size), gsub("_","", my_timer$size), my_timer$size)
my_timer$size <- ifelse(my_timer$size %in% c('2830', '02830'), 'small',
                        ifelse(my_timer$size %in% c('19983', '019983'), 'medium', 'big'))

# extra information
names_methods <- c("LR", "DT", "RF", "NB", "KNN", "SVM")
color_methods <- c("gray", "darkolivegreen", "darkgreen", "darkblue", "darkred", "darkorange")


#### Graphs results: number of variables ####

# filter just big dataset
df <- my_timer %>%
  filter(size == 'big')

# reorder dataset
df$classifier <- factor(df$classifier, levels = names_methods)

g_var <- ggplot(df, aes(x = factor(n_vars, levels = c('4', '5', '6', '7', '8', '9', '10')), 
                        y = seconds, color = classifier)) +
  geom_point(size = 3) +
  geom_line(aes(group = classifier), linewidth = 1) +
  labs(color = "Algorithm", y = 'Time (seconds)') +
  scale_color_manual(values = color_methods) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  ggtitle("Averaged computation time spent into train a single model by number of variables") +
  xlab(NULL)  # Removes the x-axis title

g_var

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time-1_model/lines_vars.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### Graphs results: dataset size ####

# filter just big dataset
df <- my_timer

# summarize by size_vars and classifier
df <- df %>% 
  group_by(size, classifier) %>% 
  summarise(seconds = mean(seconds, na.rm = TRUE)) %>% 
  ungroup()

# reorder dataset
df$classifier <- factor(df$classifier, levels = names_methods)
df$size <- factor(df$size, levels = c('small', 'medium', 'big'))

# graphs vars time 
g_size <- ggplot(df, aes(x = size, y = seconds, color = classifier)) +
  geom_point(size = 3) +
  geom_line(aes(group = classifier), linewidth = 1) +
  labs(color = "Algorithm", y = 'Time (seconds)') +
  scale_color_manual(values = color_methods) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  ggtitle("Averaged computation time spent into train a single model by amount of data") +
  xlab(NULL)  # Removes the x-axis title

g_size

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time-1_model/lines_size.png", units = 'mm', dpi = 600, width = 450, height = 300)
 

#### Graphs results: variable number and dataset size ####

# filter just big dataset
df <- my_timer

# reorder dataset
df$classifier <- factor(df$classifier, levels = names_methods)
df$size <- factor(df$size, levels = c('small', 'medium', 'big'))

# graph
g_sizevar <- ggplot(df, aes(x = factor(n_vars, levels = c('4', '5', '6', '7', '8', '9', '10')), 
                            y = classifier, fill = seconds)) +
  geom_tile() +
  scale_fill_gradient('MCC', low = "#D3D3D3", high = "#FF0000") +  # Use a red-yellow color scale for stronger colors on higher values
  xlab('') +
  ylab('') +
  ggtitle('Averaged computation time spent into train a single model by amount of data and number of variables') +
  theme_classic() + 
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 20, hjust = 0.5),
    strip.text.x = element_text(size = 15)
  ) +
  facet_wrap(~factor(size, levels = c('small', 'medium', 'big')), ncol = 3)

g_sizevar

ggsave("2_scripts/4_figures/9.4_best_model_metrics/time-1_model/heatmap_size-var.png", units = 'mm', dpi = 600, width = 450, height = 300)


#### Graphs results: grouped graphs ####

# manage graphs
g_size <- g_size + theme(plot.title = element_blank())
g_var <- g_var + theme(plot.title = element_blank())
g_sizevar <- g_sizevar + theme(plot.title = element_blank())
g1 <- ggarrange(g_size, g_var, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c('A', 'B'), hjust = 0)
ggarrange(g1, g_sizevar, ncol = 2, nrow = 1, common.legend = FALSE, legend = "bottom", widths = c(1, 1.5), labels = c('', 'C'), hjust = 0)
ggsave("2_scripts/4_figures/9.4_best_model_metrics/grouped_graphs/time.png", units = 'mm', dpi = 600, width = 600, height = 300)
