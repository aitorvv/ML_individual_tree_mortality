#------------------------------------------------------------------------------------------#
####                               Performance graphs                                   ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 09/06/2023                              #
#                              Last modification: 24/10/2023                               #
#------------------------------------------------------------------------------------------#


#### Metrics graphs ####

# The following function graph the metrics calculated for each type of analysis
# The result is a group of graphs with the value of one metric for different analysis and variables combinations
# Arguments:
  # metrics_list: lists with the same metric for all the analysis and combinations
  # n_data: lenght of combinations performed
  # names_methods: labels of the methods
  # color_methods: colors to draw each methodology always using the same
  # g_title: title of the graph
  # g_x: x label
  # g_y: y label
  # g_legend: legend title
  # g_name: graph file name (to export)
  # case_study_name: name of the case study the save the graph on the correct folder

metric_graph <- function(metrics_list, n_data, names_methods, color_methods,
                         g_title, g_x, g_y, g_legend, g_name, case_study_name){
  
  # create df to store metrics
  all_models_metrics <- as.data.frame(matrix(nrow = n_data, ncol = 0))
  
  # for each value on the list
  for(k in 1:length(metrics_list)){
    
    # store values on the main df    
    all_models_metrics <- cbind(all_models_metrics, data.frame(matrix(unlist(metrics_list[k]))))
    colnames(all_models_metrics)[k] = names_methods[k]
  }
  
  # change shape of data
  all_models_metrics$status <- 1 # content doesn't matter now
  all_models_metrics <- melt(all_models_metrics, id.var="status")
  
  
  #### Boxplot ####
  
  # plot metrics results comparing on groups of status
  graph <- ggplot(all_models_metrics, aes(x = variable, y = value)) + 
    geom_boxplot(aes(fill = variable)) + # graph type
    labs(title = g_title, # title
         x = g_x, y = g_y, # labs
         fill = g_legend) + # legend
    theme_light() + # theme
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    scale_fill_manual(values = color_methods) # colors
  
  # show graph
  # print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.3_metrics/', case_study_name, '/boxplot_', g_name, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
  
  
  #### Boxplot and density ####
  
  # plot metrics results comparing on groups of status
  graph <- ggplot(all_models_metrics, aes(x = variable, y = value)) + 
    
    # add half-violin from ggdist package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = 1,
      ## move geom to the right
      justification = -.2,
      ## remove slab interval
      .width = 0.5,
      point_colour = NA,
      # color
      aes(fill = variable)
    ) +
    
    geom_boxplot(aes(fill = variable), width = .2) + # graph type
    labs(title = g_title, # title
         x = g_x, y = g_y, # labs
         fill = g_legend) + # legend
    theme_light() + # theme
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    scale_fill_manual(values = color_methods) + # colors
    # flip positions
    coord_flip()
  
  # show graph
  # print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.3_metrics/', case_study_name, '/boxplot_density_', g_name, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
  
  
  #### Density: all togheter ####
  
  # Create a ggplot with only density plots
  graph <- ggplot(all_models_metrics, aes(x = value, colour = variable)) + 
    geom_density(lwd = 0.5, linetype = 1) + # graph type
    labs(title = g_title, # title
         x = g_x, y = g_y, # labs
         fill = g_legend) + # legend
    theme_light() + # theme
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    scale_color_manual(values = color_methods)  # colors
  
  # print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.3_metrics/', case_study_name, '/density_all_', g_name, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
  
  
  #### Density: splitted ####
  
  # all the metrics at the same time
  all_models_metrics_all <- all_models_metrics
  all_models_metrics_all$variable <- 'all classifiers'
  all_models_metrics <- rbind(all_models_metrics, all_models_metrics_all)
  
  # Create a ggplot with only density plots splitted by classifier
  graph <- ggplot(all_models_metrics, aes(x = value, colour = variable)) + 
    geom_density(lwd = 0.5, linetype = 1) + # graph type
    labs(title = g_title, # title
         x = g_x, y = g_y, # labs
         fill = g_legend) + # legend
    theme_light() + # theme
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    scale_color_manual(values = c(color_methods, 'darkviolet')) +  # colors
    facet_wrap(~ variable, scales = 'free')
  
  # print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.3_metrics/', case_study_name, '/density_split_', g_name, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}


#### Time graphs ####

# The following function graph the time spent for each type of analysis
# Arguments:
# my_timer_general: df with information of the time spent per analysis
# my_order: order selected to draw each analysis in the graph
# case_study_name: name of the case study the save the graph on the correct folder

time_graph <- function(my_timer_general, my_order, case_study_name){
  
  # plot metrics results comparing on groups of status
  graph <- ggplot(my_timer_general, aes(x = factor(msg, level = my_order), y = mins)) + 
    geom_bar(aes(fill = factor(msg, level = my_order)), stat="identity", position="dodge") + # graph type
    geom_text(aes(label = round(mins)), vjust = -0.5, color = "black", size = 5) +  # Display bar values on top of bars
    labs(title = paste('Time sent on analysis per classifier for ', n_data, ' models tested', sep = ''), # title
         x = '', y = 'Time (mins)', # labs
         fill = g_legend) + # legend
    theme_light() + # theme
    theme(plot.title = element_text(hjust = 0.5)) + # center title
    scale_fill_manual(values = color_methods) # colors
  
  # show graph
  # print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.3_metrics/', case_study_name, '/time_analysis.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}


#### Time graphs ####

# The following function select the best model of each analysis and study case based on the provided metric
# Arguments:
# names_methods: list with the names of the analysis
# all_methods_acc: list with the selected metric of all the models for all the analysis
# my_combis: variables combinations - number of models tested

# function to find the best model
find_best_model <- function(names_methods, all_methods_acc, my_combis){
  
  # counter
  count <- 0
  # df to add metrics summary
  best_acc <- as.list(rep(0, length(all_methods_acc)))
  best_model <- best_acc
  
  # for each method...
  for (method in all_methods_acc){
    count <- count + 1
    
    # for each combination...
    for (k in 1:length(my_combis)){
      
      # if a metric is better than the previous registers, then it keeps on the list
      if(best_acc[[count]] < method[[k]]){ 
        best_acc[[count]] <- method[[k]]
        best_model[[count]] <- k
        
        # else, the previous one stays in the list
      } else { 
        best_acc[[count]] <- best_acc[[count]]
      }
    }
  }
  # join results in a unique df
  the_bests <- tibble(names_methods, best_model, best_acc)
  return(the_bests)
}


#### Best models: all metrics graphs ####

# The following function graph all the metrics for the best model on each case study and analysis
# Arguments:
# names_methods: list with the names of the analysis
# case_study_name: name of the case study the save the graph on the correct folder

# function to find the best model
graph_best_model <- function(best_compilation, case_study_name){

  # bar graph
  graph <- ggplot(best_compilation, aes(x = names_methods, y = as.numeric(best_acc), 
                                        fill = Metrics)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = 'Metrics comparison for the best model of each classifier',
         x = 'Classifier', y = 'Metric value')

  # show graph  
  # print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.3_metrics/', case_study_name, '/best_model_all_metrics.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}
