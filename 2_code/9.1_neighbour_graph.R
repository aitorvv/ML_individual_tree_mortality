#------------------------------------------------------------------------------------------#
####                          Methodology neighbourhood graph                           ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 02/06/2023                              #
#                              Last modification: 02/06/2023                               #
#------------------------------------------------------------------------------------------#

library(ggplot2)

setwd('ML_individual_tree_mortality/')

trees <- read.csv('1_data/tmp_DEN/1_neighborhood/trees_r33.csv', sep = ',')

# filter one plot
p1 <- trees[trees$PLOT_ID_time == '5_1_1985', ]


ggplot(p1, aes(x, y)) +
  geom_point(aes(size = height)) + 
  theme_light() +
  scale_size_continuous(range = c(0.5, 2.5)) +
  labs(# title =  'Mortality rate evolution among field measurements',
       x = 'X coordinate (m)', y = 'Y coordinate (m)') + # labels   
  theme(legend.position = 'none')  
# scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) # origin 0 0
