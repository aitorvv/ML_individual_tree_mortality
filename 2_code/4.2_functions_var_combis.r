#------------------------------------------------------------------------------------------#
####                               Variables selection                                
#                                                                                          #
#                            Aitor Vázquez Veloso, 17/10/2023                              #
#                              Last modification: 27/10/2023                               #
#------------------------------------------------------------------------------------------#

# On this script different groups of variables were created and also functions to obtain all of these variables
# combinations at three different levels, considering different users data availability.


#### Create variable groups ####

# important in all models
f_basics <- c('dead', 'dbh', 'SI_local')
f_basics_slend <- c('dead', 'dbh', 'slenderness', 'SI_local')

# just one of the following
f_competition <- c('bal_subplot', 'Hegyi', 'G_local', 'N', 'SDI')

# choose one combination
f_growth1 <- c('i_annual_g')
f_growth2 <- c('i_year')
f_growth3 <- c('i_annual_g', 'i_year') 

# one or both can be included
f_clima1 <- c('M')
f_clima2 <- c('SPEI6_aug')
f_clima3 <- c('M', 'SPEI6_aug')

# social position variables
f_social1 <- c('rel_dbh_Dg')
f_social2 <- c('rel_h_Hm')


#### Combination with basics, competition, growth, clima and social position ####

my_combis_extreme <- function(f_basics, f_basics_slend, f_competition,
                           f_growth1, f_growth2, f_growth3, 
                           f_clima1, f_clima2, f_clima3,
                           f_social1, f_social2){
  
  ## get feature combinations
  my_combis_a <- list()
  
  for (a in 1:2){
    
    # basics
    if(a == 1){
      # competition
      for (k in 1:length(f_competition)){
        # growth 1 combination
        name <- paste('comb_', 'g1_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth1, f_competition[k])
        my_combis_a[[name]] <- tmp
        # growth 2 combination
        name_a <- paste('comb_', 'g2_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth2, f_competition[k])
        my_combis_a[[name_a]] <- tmp
        # growth 3 combination
        name_a <- paste('comb_', 'g3_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth3, f_competition[k])
        my_combis_a[[name_a]] <- tmp
      }
      
    } else {
      
      for (k in 1:length(f_competition)){
        # growth 1 combination
        name <- paste('comb_sl_', 'g1_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth1, f_competition[k])
        my_combis_a[[name]] <- tmp
        # growth 2 combination
        name_a <- paste('comb_sl_', 'g2_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth2, f_competition[k])
        my_combis_a[[name_a]] <- tmp
        # growth 3 combination
        name_a <- paste('comb_sl_', 'g3_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth3, f_competition[k])
        my_combis_a[[name_a]] <- tmp
      }
    }
  }
  
  my_combis_c <- list()
  my_combis_a_names <- names(my_combis_a)
  count <- 0
  
  for (combi in my_combis_a){
    
    count = count + 1
    # clima
    for (c in 1:3){
      
      if (c == 1){  
        
        name <- paste(my_combis_a_names[count], '_cl', c, sep = '')
        tmp <- c(my_combis_a[[count]], f_clima1)
        my_combis_c[[name]] <- tmp
      } else if (c == 2){  
        
        name <- paste(my_combis_a_names[count], '_cl', c, sep = '')
        tmp <- c(my_combis_a[[count]], f_clima2)
        my_combis_c[[name]] <- tmp
      } else if (c == 3){  
        
        name <- paste(my_combis_a_names[count], '_cl', c, sep = '')
        tmp <- c(my_combis_a[[count]], f_clima3)
        my_combis_c[[name]] <- tmp
      } 
    }
  }
  
  my_combis <- list()
  my_combis_s_names <- names(my_combis_c)
  count <- 0
  
  for (combi in my_combis_c){
    
    count = count + 1
    # social
    for (c in 1:2){
      
      if (c == 1){  
        
        name <- paste(my_combis_s_names[count], '_s', c, sep = '')
        tmp <- c(my_combis_c[[count]], f_social1)
        my_combis[[name]] <- tmp
      } else{  
        
        name <- paste(my_combis_s_names[count], '_s', c, sep = '')
        tmp <- c(my_combis_c[[count]], f_social2)
        my_combis[[name]] <- tmp
      } 
    }
  }
  
  # return combis
  return(my_combis)
}



#### Combination with basics, competition, growth and clima ####

my_combis_hard <- function(f_basics, f_basics_slend, f_competition,
                           f_growth1, f_growth2, f_growth3, 
                           f_clima1, f_clima2, f_clima3){
  
  ## get feature combinations
  my_combis_a <- list()
  
  for (a in 1:2){
    
    # basics
    if(a == 1){
      # competition
      for (k in 1:length(f_competition)){
        # growth 1 combination
        name <- paste('comb_', 'g1_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth1, f_competition[k])
        my_combis_a[[name]] <- tmp
        # growth 2 combination
        name_a <- paste('comb_', 'g2_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth2, f_competition[k])
        my_combis_a[[name_a]] <- tmp
        # growth 3 combination
        name_a <- paste('comb_', 'g3_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth3, f_competition[k])
        my_combis_a[[name_a]] <- tmp
      }
      
    } else {
      
      for (k in 1:length(f_competition)){
        # growth 1 combination
        name <- paste('comb_sl_', 'g1_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth1, f_competition[k])
        my_combis_a[[name]] <- tmp
        # growth 2 combination
        name_a <- paste('comb_sl_', 'g2_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth2, f_competition[k])
        my_combis_a[[name_a]] <- tmp
        # growth 3 combination
        name_a <- paste('comb_sl_', 'g3_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth3, f_competition[k])
        my_combis_a[[name_a]] <- tmp
      }
    }
  }
  
  my_combis_c <- list()
  my_combis_a_names <- names(my_combis_a)
  count <- 0
  
  for (combi in my_combis_a){
    
    count = count + 1
    # clima
    for (c in 1:3){
      
      if (c == 1){  
        
        name <- paste(my_combis_a_names[count], '_cl', c, sep = '')
        tmp <- c(my_combis_a[[count]], f_clima1)
        my_combis_c[[name]] <- tmp
      } else if (c == 2){  
        
        name <- paste(my_combis_a_names[count], '_cl', c, sep = '')
        tmp <- c(my_combis_a[[count]], f_clima2)
        my_combis_c[[name]] <- tmp
      } else if (c == 3){  
        
        name <- paste(my_combis_a_names[count], '_cl', c, sep = '')
        tmp <- c(my_combis_a[[count]], f_clima3)
        my_combis_c[[name]] <- tmp
      } 
    }
  }
  
  my_combis <- my_combis_c
  
  # return combis
  return(my_combis)
}


# a <- my_combis_hard(f_basics, f_basics_slend, f_competition,
#                 f_growth1, f_growth2, f_growth3, 
#                 f_clima1, f_clima2, f_clima3)


#### Combination with basics, competition and growth ####

my_combis_medium <- function(f_basics, f_basics_slend, f_competition,
                             f_growth1, f_growth2, f_growth3, 
                             f_clima1, f_clima2, f_clima3){
  
  ## get feature combinations
  my_combis_a <- list()
  
  for (a in 1:2){
    
    # basics
    if(a == 1){
      # competition
      for (k in 1:length(f_competition)){
        # growth 1 combination
        name <- paste('comb_', 'g1_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth1, f_competition[k])
        my_combis_a[[name]] <- tmp
        # growth 2 combination
        name_a <- paste('comb_', 'g2_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth2, f_competition[k])
        my_combis_a[[name_a]] <- tmp
        # growth 3 combination
        name_a <- paste('comb_', 'g3_', 'co', k, sep = '')
        tmp <- c(f_basics, f_growth3, f_competition[k])
        my_combis_a[[name_a]] <- tmp
      }
      
    } else {
      
      for (k in 1:length(f_competition)){
        # growth 1 combination
        name <- paste('comb_sl_', 'g1_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth1, f_competition[k])
        my_combis_a[[name]] <- tmp
        # growth 2 combination
        name_a <- paste('comb_sl_', 'g2_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth2, f_competition[k])
        my_combis_a[[name_a]] <- tmp
        # growth 3 combination
        name_a <- paste('comb_sl_', 'g3_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_growth3, f_competition[k])
        my_combis_a[[name_a]] <- tmp
      }
    }
  }
  
  my_combis <- my_combis_a
  
  # return combis
  return(my_combis)
}

# b <- my_combis_medium(f_basics, f_basics_slend, f_competition,
#                     f_growth1, f_growth2, f_growth3, 
#                     f_clima1, f_clima2, f_clima3)


#### Combination with basics and competition ####

my_combis_easy <- function(f_basics, f_basics_slend, f_competition,
                             f_growth1, f_growth2, f_growth3, 
                             f_clima1, f_clima2, f_clima3){
  
  ## get feature combinations
  my_combis_a <- list()
  
  for (a in 1:2){
    
    # basics
    if(a == 1){
      # competition
      for (k in 1:length(f_competition)){

        name <- paste('comb_', 'co', k, sep = '')
        tmp <- c(f_basics, f_competition[k])
        my_combis_a[[name]] <- tmp

      }
      
    } else {
      
      for (k in 1:length(f_competition)){
        
        name <- paste('comb_sl_', 'co', k, sep = '')
        tmp <- c(f_basics_slend, f_competition[k])
        my_combis_a[[name]] <- tmp

      }
    }
  }
  
  my_combis <- my_combis_a
  
  # return combis
  return(my_combis)
}
