#------------------------------------------------------------------------------------------#
####                            Split variable combinations
#                                                                                          #
#                            Aitor Vázquez Veloso, 17/10/2023                              #
#                              Last modification: 27/10/2023                               #
#------------------------------------------------------------------------------------------#


setwd('ML_individual_tree_mortality/')

# functions 
source('2_code/4.2_functions_var_combis.r')

# combination 1
my_combis_easy <- my_combis_easy(f_basics, f_basics_slend, f_competition,
                            f_growth1, f_growth2, f_growth3, 
                            f_clima1, f_clima2, f_clima3)

saveRDS(my_combis_easy, file = '1_data/tmp_DEN/4_datasets/combis_easy.RDS')

# combination 2
my_combis_medium <- my_combis_medium(f_basics, f_basics_slend, f_competition,
                                 f_growth1, f_growth2, f_growth3, 
                                 f_clima1, f_clima2, f_clima3)

saveRDS(my_combis_medium, file = '1_data/tmp_DEN/4_datasets/combis_medium.RDS')

# combination 3
my_combis_hard <- my_combis_hard(f_basics, f_basics_slend, f_competition,
                                 f_growth1, f_growth2, f_growth3, 
                                 f_clima1, f_clima2, f_clima3)

saveRDS(my_combis_hard, file = '1_data/tmp_DEN/4_datasets/combis_hard.RDS')

# combination 4
my_combis_extreme <- my_combis_extreme(f_basics, f_basics_slend, f_competition,
                                       f_growth1, f_growth2, f_growth3, 
                                       f_clima1, f_clima2, f_clima3,
                                       f_social1, f_social2)

saveRDS(my_combis_extreme, file = '1_data/tmp_DEN/4_datasets/combis_extreme.RDS')
