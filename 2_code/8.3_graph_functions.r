#------------------------------------------------------------------------------------------#
####                           Case studies: graph functions                            ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 27/10/2023                              #
#                              Last modification: 06/11/2023                               #
#------------------------------------------------------------------------------------------#


pointline_by_cs <- function(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title, min_shadow, max_shadow){

  graph <- ggplot(df_graph, aes(x = x_axis, y = y_axis, color = fill)) +
    geom_point(size = 3) +
    # Adding shaded background for the desired class
    geom_rect(
      aes(xmin = min_shadow, xmax = max_shadow,
          ymin = -Inf, ymax = Inf),
      fill = "lightgray", alpha = 0.2, inherit.aes = FALSE
    ) +
    geom_point(size = 3) +
    geom_line(aes(group = fill), size = 1) +
    labs(color = "Classifiers", y = g_y) +
    scale_color_manual(values = color_groups) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 15),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 20),
      plot.title = element_text(size = 20, hjust = 0.5)  
    ) +
    ggtitle(g_title) +
    xlab(NULL)

  
  # show graph
  print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.4_best_model_metrics/', graph_title, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
  
  # return graph
  return(graph)
}

bar_by_cs <- function(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title){
  
  # bar graph comparing classifiers
  graph <- ggplot(df_graph, aes(x = x_axis, y = y_axis, fill = fill)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    theme_light() +
    #theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    #labs(title = g_title, x = g_x, y = g_y, fill = g_legend) +
    labs(title = g_title, y = g_y, fill = g_legend) +
    scale_fill_manual(values = color_groups) + # colors
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    facet_wrap(~ df_graph$facet)
  
  # show graph
  print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.4_best_model_metrics/', graph_title, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}


bar_by_cs_size_var <- function(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title){
  
  # bar graph comparing classifiers
  graph <- ggplot(df_graph, aes(x = x_axis, y = y_axis, fill = fill)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    theme_light() +
    #theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    #labs(title = g_title, x = g_x, y = g_y, fill = g_legend) +
    labs(title = g_title, y = g_y, fill = g_legend) +
    scale_fill_manual(values = color_groups) + # colors
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    facet_wrap(~ df_graph$facet, ncol = 4, nrow = 5)
  
  # show graph
  print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.4_best_model_metrics/', graph_title, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}

bar_by_classifiers <- function(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title){
  
  # bar graph comparing classifier performance under each case study
  graph <- ggplot(df_graph, aes(x = x_axis, y = y_axis, fill = fill)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    theme_light() +
    #theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    #labs(title = g_title, x = g_x, y = g_y, fill = g_legend) +
    labs(title = g_title, y = g_y, fill = g_legend) +
    scale_fill_manual(values = color_groups) + # colors
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    facet_wrap(~ df_graph$facet, scales = 'free')
  
  # show graph
  print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.4_best_model_metrics/', graph_title, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}


time_bar_by_cs <- function(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title){
  
  # bar graph comparing classifiers
  graph <- ggplot(df_graph, aes(x = x_axis, y = y_axis, fill = fill)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    theme_light() +
    #theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    #labs(title = g_title, x = g_x, y = g_y, fill = g_legend) +
    labs(title = g_title, y = g_y, fill = g_legend) +
    scale_fill_manual(values = color_groups) + # colors
    scale_y_continuous(breaks = seq(0, max(y_axis), by = 30)) +
    facet_wrap(~ df_graph$facet, scales = 'free')
  
  # show graph
  print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.4_best_model_metrics/', graph_title, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}

time_bar_by_classifiers <- function(df_graph, x_axis, y_axis, fill, g_title, g_x, g_y, g_legend, color_groups, graph_title){
  
  # bar graph comparing classifier performance under each case study
  graph <- ggplot(df_graph, aes(x = x_axis, y = y_axis, fill = fill)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    theme_light() +
    #theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.title.x = element_blank()) +
    #labs(title = g_title, x = g_x, y = g_y, fill = g_legend) +
    labs(title = g_title, y = g_y, fill = g_legend) +
    scale_fill_manual(values = color_groups) + # colors
    scale_y_continuous(breaks = seq(0, max(y_axis), by = 30)) +
    facet_wrap(~ df_graph$facet, scales = 'free')
  
  # show graph
  print(graph)
  
  # save graph
  my_path <- paste('3_figures/tmp_figures/9.4_best_model_metrics/', graph_title, '.png', sep = '')
  ggsave(filename = my_path, device = 'png', units = 'mm', dpi = 300, width = 300, height = 300)
}