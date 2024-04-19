#------------------------------------------------------------------------------------------#
####                         Trees neighborhood: functions                              ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 31/05/2023                              #
#                              Last modification: 13/06/2023                               #
#------------------------------------------------------------------------------------------#


#### auxiliar function to round ####

round_up <- function(x, base){
  #Args.:
  #  - x = variable to round
  #  - base = integer value to round the x
  base * round((x + (base/2.01))/base)
}   

#### function to manage initial data ####

manage_data <- function(df, radius_height){
    
  # to numeric
  #df$dbh <- as.numeric(sub(",", ".", df$dbh, fixed = TRUE))
  #df$g <- as.numeric(sub(",", ".", df$g, fixed = TRUE))
  #df$x <- as.numeric(sub(",", ".", df$x, fixed = TRUE))
  #df$y <- as.numeric(sub(",", ".", df$y, fixed = TRUE))
  #df$height <- as.numeric(sub(",", ".", df$height, fixed = TRUE))
  #df$search_radius <- as.numeric(sub(",", ".", df$search_radius, fixed = TRUE))

  # adaptation
  df$species <- df$Artcode
  df$age <- df$AGE
  df$removed <- ifelse(df$removed == 1, 'yes', 'no')
  df$dead <- ifelse(df$dead == 1, 'yes', 'no')
  df$thinned <- ifelse(df$thinned == 1, 'yes', 'no')
  #df$thinning <- ifelse(df$thinning == 'A_Grad', 'no', 'yes')
  df$search_radius <- df$height*radius_height
  
  # calculate the expan for the subplot
  df$expan_subplot <- 10000/(pi*(df$search_radius**2))
  
  # expan_subplot for trees without height (not the main species)
  # --> not used, that was an idea to avoid error on Ho calculation
  #df_tmp <- df[df$Artcode != 10, ]
  
  #for(tree in df_tmp$TREE_ID_time){ 
    # select trees on the same plot and year but from the main species
  #  tree <- df_tmp[df_tmp$TREE_ID_time == tree, ]
  #  trees_main_species <- df[df$PLOT_ID_time == tree$PLOT_ID_time, ]
  #  trees_main_species <- trees_main_species[trees_main_species$Artcode == 10, ]
    # choose the value of expan_subplot and copy it to our tree
  #  similar_tree <- trees_main_species[which.min(abs(tree$dbh - trees_main_species$dbh)),]
  #  tree$expan_subplot <- similar_tree$expan_subplot
  #  }

  # extra variables
  #df$INVENTORY_ID <- ifelse(df$site == 612, 'FFB', 'WEI')
  #df$PLOT_ID <- paste(df$site, df$plot, sep = '_')
  #df$PLOT_ID_time <- paste(df$PLOT_ID, '_', df$year, sep = '')
  #df$TREE_ID <- paste(df$PLOT_ID, df$Nr, sep = '_')
  #df$TREE_ID_time <- paste(df$TREE_ID, '_', df$year, sep = '')
  #df$expan <- pi*(search_radius**2)  # each plot is 900 m2 area, but the subplot we are going to create is different for each tree
  
  # calculate plot size variables
  all_plots_area <- tibble()
  
  for (plot in unique(df$PLOT_ID)){
    # get plot data
    tmp_plot <- df[df$PLOT_ID == plot, ]
    tmp_plot <- tmp_plot[!is.na(tmp_plot$x) & !is.na(tmp_plot$y), ]
    # calculate max and min values for each coordinate
    x_min <- 0
    x_max <- round_up(max(tmp_plot$x), 5)
    y_min <- 0
    y_max <- round_up(max(tmp_plot$y), 5)
    # calculate plot area and expan
    plot_area <- x_max*y_max
    expan_plot <- 10000/plot_area
    # add information   
    all_plots_area_tmp <- tibble(tmp_plot$PLOT_ID, x_min, x_max, y_min, y_max, plot_area, expan_plot)
    all_plots_area_tmp <- all_plots_area_tmp[!duplicated(all_plots_area_tmp), ]
    all_plots_area <- rbind(all_plots_area, all_plots_area_tmp)
  }
  
  # merge dataframes
  df <- merge(df, all_plots_area, by.x = 'PLOT_ID', by.y = 'tmp_plot$PLOT_ID')
  
  # some extra tree variables
  df$g <- pi*((df$dbh/2)**2)
  df$slenderness <- df$height*100/df$dbh
  
  # time and growth between measurements
  increments <- tibble(
    INVENTORY_ID = character(),
    PLOT_ID = character(),
    TREE_ID = character(),
    TREE_ID_time = character(),
    year = numeric(),
    i_year = numeric(),
    i_dbh = numeric(),
    i_g = numeric(),
    i_annual_dbh = numeric(),
    i_annual_g = numeric()
  )
  
  for(inventory in unique(df$INVENTORY_ID)){
    # df for each inventory ID and list of years
    df_inv <- df[df$INVENTORY_ID == inventory, ]
    
    for(plot in unique(df_inv$PLOT_ID)){
      # df for each plot
      df_plot <- df_inv[df_inv$PLOT_ID == plot, ]
      
      for(tree in unique(df_plot$TREE_ID)){
        # df for each tree ID and list of codes and dbh
        df_plot_tree <- df_plot[df_plot$TREE_ID == tree, ]
        dbh_list <- df_plot_tree$dbh
        years_list <- unique(df_plot_tree$year)
        tree_time <- unique(df_plot_tree$TREE_ID_time)
        
        for (year in 1:length(unique(df_plot_tree$year))){
          # first year has no increments, following are calculates
          if(year == 1){
            i_dbh <- 0
            i_g <- 0
            i_year <- 0
            i_annual_dbh <- 0
            i_annual_g <- 0
          } else {
            i_dbh <- df_plot_tree$dbh[year] - df_plot_tree$dbh[year-1]
            i_g <- df_plot_tree$g[year] - df_plot_tree$g[year-1]
            i_year <- df_plot_tree$year[year] - df_plot_tree$year[year-1]
            i_annual_dbh <- i_dbh/i_year
            i_annual_g <- i_g/i_year
          }
          
          # additional IDs and select currect year
          plot <- unique(df_plot_tree$PLOT_ID)
          tree <- unique(df_plot_tree$TREE_ID)
          current_year <- years_list[year]
          # temporal dataframe
          increment_j <- data.frame(inventory, plot, tree, tree_time[year], current_year, 
                                    i_year, i_dbh, i_annual_dbh, i_g, i_annual_g)
          #increment_j <- increment_j[!duplicated(increment_j), ]
          names(increment_j) <- c("INVENTORY_ID", "PLOT_ID", "TREE_ID", "TREE_ID_time", 
                                  "year", "i_year", "i_dbh", "i_annual_dbh", "i_g", "i_annual_g")
          # add temporal dataframe to the main one
          increments <- rbind(increments, increment_j)
        }
      }
    }
  }
  
  # merge new df with increments and the general one
  df <- merge(df, increments, by = c('INVENTORY_ID', 'PLOT_ID', 'TREE_ID', 'TREE_ID_time', 'year'), 
               all.x = TRUE, all.y = FALSE)

  # bal calculation
  df <- df[order(df$PLOT_ID_time, # sorting trees
                 df$dbh, 
                 decreasing = TRUE), ]
  
  bal_data <- data.frame()
  
  for (plot in unique(df$PLOT_ID_time)){ # for each plot
    
    my_trees <- df[df$PLOT_ID_time %in% plot, ] # select trees
    bal <- 0 # first tree
    list_trees_objective <- data.frame() # list for results
    
    for (tree in my_trees$TREE_ID_time){ # for each tree
      # selected tree
      tree_objective <- my_trees[my_trees$TREE_ID_time == tree, ] 
      # bal assignment
      tree_objective$bal_plot <- bal
      # adding to the list
      list_trees_objective <- rbind(list_trees_objective, tree_objective) 
      # updating bal to the next tree
      bal <- bal + tree_objective$g*tree_objective$expan_plot/10000
      
    }
    # adding tree to the list
    bal_data <- rbind(bal_data, list_trees_objective) 
  }
  
  # clean database
  df <- select(bal_data,
               c(INVENTORY_ID, PLOT_ID, PLOT_ID_time, TREE_ID, TREE_ID_time, site, plot, year,
                 i_year, removed, thinned, dead, species, x, x_min, x_max, y, y_min, y_max, z, 
                 expan_plot, expan_subplot, age, dbh, i_dbh, i_annual_dbh, g, i_g, 
                 i_annual_g, bal_plot, height, hcb, slenderness, search_radius))
  return(df)
}


#### function to find neighbors ####

get_neighbors <- function(plot){
  
  # get geometry
  sf_plot <- st_as_sf(plot, coords = c('x', 'y'), dim = "XY")
  
  # calculate distances between trees via the st_distance function from the
  # simple feature package (sf)
  dist_plot <- sf::st_distance(sf_plot$geometry) |>
    # from matrix to tibble
    tibble::as_tibble() |>
    # set names to tree id's (potential competitor trees)
    setNames(plot$TREE_ID_time) |>
    # add column with tree id's of reference trees
    dplyr::mutate(TREE_ID_time = plot$TREE_ID_time) |>
    # for cosmetic rearrange columns
    dplyr::select(TREE_ID_time, where(is.numeric)) |>
    # from wide to long
    tidyr::pivot_longer(
      cols = !TREE_ID_time,
      names_to = "tree_id_neighbor",
      values_to = "distance"
    ) |>
    # again cosmetic
    dplyr::arrange(as.character(TREE_ID_time), distance) |>
    # remove the rows in which competitor equals reference tree
    dplyr::filter(TREE_ID_time != tree_id_neighbor)
  
  # include distance to the circle limit
  search_distance <- select(plot, c(TREE_ID_time, search_radius))
  dist_plot <- merge(dist_plot, search_distance, by = 'TREE_ID_time')
  
  return(dist_plot)
}


#### function to create subplots and calculate features ####

get_subplots <- function(plot, dist_plot){
  
  # variables to fill with data
  subplot_stats <- as_tibble(as.data.frame(matrix(nrow = 0, ncol = 0)))
  neighborhood_stats <- as_tibble(as.data.frame(matrix(nrow = 0, ncol = 0)))
  
  for (j in (1:nrow(plot))) {  # for each tree
    
    # select center tree
    center_tree <- plot[j, ]

    # filter trees that are not our main specie
    if(center_tree$species != 10){
      
      cat('Tree', center_tree$TREE_ID_time, 
          'is not Picea abies. Species code:', center_tree$species, '\n')

      } else {

      # get neighbors
      neighbors <- dist_plot |>
        filter(
          TREE_ID_time == center_tree$TREE_ID_time,
          distance <= center_tree$search_radius
        ) |>
        select("tree_id_neighbor", "distance")
      
      # get center tree geometry
      center_tree_temp <- st_as_sf(center_tree, coords = c('x', 'y'), dim = "XY")
      
      # select outline polygon according with each site
      x <- c(center_tree_temp$x_min, center_tree_temp$x_min,
             center_tree_temp$x_max, center_tree_temp$x_max)
      y <- c(center_tree_temp$y_min, center_tree_temp$y_max,
             center_tree_temp$y_max, center_tree_temp$y_min)
  
      # create corners and outline as polygon
      corners <- as_tibble(cbind(x, y))
      
      outline <- corners |>
        dplyr::bind_rows(
          dplyr::slice(corners, 1L)
        ) |>
        as.matrix()
      
      outline <- sf::st_polygon(list(outline))
      
      # dealing with border trees
      center_tree_temp <- center_tree_temp |>
        
        # variable to estimate subplot area in and out of the borders
        dplyr::mutate(
          exp_circle_sect = sf::st_buffer(geometry, dist = search_radius) %>%
            sf::st_cast(to = "LINESTRING") %>%
            sf::st_intersection(outline),
          
          # weight variable to extrapolate missing information due to border effect
          weight_borders = 2 * search_radius * pi / sf::st_length(exp_circle_sect))
      
      # extract variable
      weight_borders <- center_tree_temp$weight_borders
      
      # skip little modifications of final values due to decimals
      if(0.99 < weight_borders & weight_borders < 1.01){
        weight_borders <- round(weight_borders, 2)
      }
      
      # filter neighbors in original dataset and include center tree
      neighborhood_j <- plot |> filter(TREE_ID_time %in% neighbors$tree_id_neighbor)
  
      # merge distance data
      neighborhood_j <- merge(neighborhood_j, neighbors, by.x = 'TREE_ID_time', by.y = 'tree_id_neighbor')
      center_tree$distance <- 0
      
      # divide between data with and without center tree
      neighborhood_no_center <- neighborhood_j
      neighborhood_j <- rbind(center_tree, neighborhood_j)
  
      # add center tree code
      neighborhood_j$TREE_ID_center <- center_tree$TREE_ID_time

      # get neighborhood statistics
      subplot_stats_j <- tibble(
        # CODES and basics
        INVENTORY_ID = unique(neighborhood_j$INVENTORY_ID),
        PLOT_ID = unique(neighborhood_j$PLOT_ID_time), 
        SUBPLOT_ID = unique(neighborhood_j$TREE_ID_center),
        AGE = unique(neighborhood_j$age),
        # variables
        N = sum(weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), na.rm = TRUE), # count(neighborhood_j), 
        N_ha = sum(weight_borders*(neighborhood_j$expan_subplot), na.rm = TRUE), 
        G = sum(weight_borders*(neighborhood_j$g)/10000, na.rm = TRUE),
        G_local = sum(weight_borders*(neighborhood_no_center$g)/10000, na.rm = TRUE), # (Steneker and Jarvis [1963])
        G_ha = sum(weight_borders*(neighborhood_j$g*neighborhood_j$expan_subplot)/10000, na.rm = TRUE),
        # auxiliar variables
        SUM_DBH = sum(weight_borders*(neighborhood_j$dbh), na.rm = TRUE), 
        SUM_H = sum(weight_borders*(neighborhood_j$height), na.rm = TRUE), 
        # diametric classes
        SD_0_5 = sum(ifelse(neighborhood_j$dbh <= 5, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_5_10 = sum(ifelse(neighborhood_j$dbh > 5 & neighborhood_j$dbh <= 10, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_10_15 = sum(ifelse(neighborhood_j$dbh > 10 & neighborhood_j$dbh <= 15, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_15_20 = sum(ifelse(neighborhood_j$dbh > 15 & neighborhood_j$dbh <= 20, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_20_25 = sum(ifelse(neighborhood_j$dbh > 20 & neighborhood_j$dbh <= 25, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_25_30 = sum(ifelse(neighborhood_j$dbh > 25 & neighborhood_j$dbh <= 30, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_30_35 = sum(ifelse(neighborhood_j$dbh > 30 & neighborhood_j$dbh <= 35, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_35_40 = sum(ifelse(neighborhood_j$dbh > 35 & neighborhood_j$dbh <= 40, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_40_45 = sum(ifelse(neighborhood_j$dbh > 40 & neighborhood_j$dbh <= 45, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        SD_45_50 = sum(ifelse(neighborhood_j$dbh > 45 & neighborhood_j$dbh <= 50, weight_borders*(neighborhood_j$expan_subplot/neighborhood_j$expan_subplot), 0), na.rm = TRUE),
        # Hegyi Index - Hegyi, 1974
        Hegyi = sum(weight_borders*(neighborhood_no_center$dbh/(center_tree$dbh*neighborhood_no_center$distance)), na.rm = TRUE)
        )
      
      # bal calculation
      bal_trees <- neighborhood_j[neighborhood_j$dbh > center_tree$dbh, ]
      neighborhood_j$bal_subplot <- weight_borders * sum(bal_trees$dbh) / 10000 # m2/ha
      neighborhood_j$bal_subplot_ha <- weight_borders * sum(bal_trees$dbh * bal_trees$expan_subplot) / 10000 # m2/ha
      
      # get neighbourhood stats by species
      subplot_sp_stats_j <- plyr::ddply(neighborhood_j, c('species'), summarise, 
                                  # CODES
                                  INVENTORY_ID = unique(INVENTORY_ID),
                                  PLOT_ID = unique(PLOT_ID_time), 
                                  SUBPLOT_ID = unique(TREE_ID_center),
                                  # variables
                                  G_ha_sp = sum(weight_borders*(g*expan_subplot)/10000, na.rm = TRUE), 
                                  G_sp = sum(weight_borders*(g)/10000, na.rm = TRUE), 
                                  N_ha_sp = sum(weight_borders*(expan_subplot), na.rm = TRUE),
                                  N_sp = sum(weight_borders*(expan_subplot/expan_subplot), na.rm = TRUE),
                                  # auxiliar variables
                                  SUM_DBH_sp = sum(weight_borders*(dbh), na.rm = TRUE), 
                                  SUM_H_sp = sum(weight_borders*(height), na.rm = TRUE),
      )
      
      # organize information by PLOT_ID and G
      subplot_sp_stats_j <- subplot_sp_stats_j %>%
        arrange(SUBPLOT_ID, -G_sp)
      
      # separate variables by species
      subplot_sp_stats_j$sp_1 <- subplot_sp_stats_j$species[1]
      subplot_sp_stats_j$sp_2 <- subplot_sp_stats_j$species[2] 
      subplot_sp_stats_j$sp_3 <- subplot_sp_stats_j$species[3] 
      subplot_sp_stats_j$G_ha_sp_1 <- subplot_sp_stats_j$G_ha_sp[1]
      subplot_sp_stats_j$G_ha_sp_2 <- subplot_sp_stats_j$G_ha_sp[2]
      subplot_sp_stats_j$G_ha_sp_3 <- subplot_sp_stats_j$G_ha_sp[3]
      subplot_sp_stats_j$G_sp_1 <- subplot_sp_stats_j$G_sp[1]
      subplot_sp_stats_j$G_sp_2 <- subplot_sp_stats_j$G_sp[2]
      subplot_sp_stats_j$G_sp_3 <- subplot_sp_stats_j$G_sp[3]
      subplot_sp_stats_j$N_ha_sp_1 <- subplot_sp_stats_j$N_ha_sp[1]
      subplot_sp_stats_j$N_ha_sp_2 <- subplot_sp_stats_j$N_ha_sp[2]
      subplot_sp_stats_j$N_ha_sp_3 <- subplot_sp_stats_j$N_ha_sp[3]
      subplot_sp_stats_j$N_sp_1 <- subplot_sp_stats_j$N_sp[1]
      subplot_sp_stats_j$N_sp_2 <- subplot_sp_stats_j$N_sp[2]
      subplot_sp_stats_j$N_sp_3 <- subplot_sp_stats_j$N_sp[3]
      subplot_sp_stats_j$SUM_DBH_sp_1 <- subplot_sp_stats_j$SUM_DBH_sp[1]
      subplot_sp_stats_j$SUM_DBH_sp_2 <- subplot_sp_stats_j$SUM_DBH_sp[2]
      subplot_sp_stats_j$SUM_DBH_sp_3 <- subplot_sp_stats_j$SUM_DBH_sp[3] 
      subplot_sp_stats_j$SUM_H_sp_1 <- subplot_sp_stats_j$SUM_H_sp[1]
      subplot_sp_stats_j$SUM_H_sp_2 <- subplot_sp_stats_j$SUM_H_sp[2]
      subplot_sp_stats_j$SUM_H_sp_3 <- subplot_sp_stats_j$SUM_H_sp[3]
      
      # delete duplicated data
      subplot_sp_stats_j <- subplot_sp_stats_j[!duplicated(subplot_sp_stats_j$SUBPLOT_ID), ]
      subplot_sp_stats_j <- select(subplot_sp_stats_j, -c(species, INVENTORY_ID, PLOT_ID, N_sp, N_ha_sp, G_sp, G_ha_sp, SUM_DBH_sp, SUM_H_sp))
      
      # merge species and general information
      subplot_stats_j <- merge(subplot_stats_j, subplot_sp_stats_j, all = TRUE, by = 'SUBPLOT_ID')
      
      # include weight_borders on subplot_stats
      subplot_stats_j$weight_borders <- weight_borders
      
      # add data to df_bal
      neighborhood_stats <- rbind(neighborhood_stats, neighborhood_j)
      subplot_stats <- rbind(subplot_stats, subplot_stats_j)
    } # if statement
  } # internal for loop
  stats <- list('neighborhood' = neighborhood_stats, 'subplot' = subplot_stats)
  return(stats)
} 


#### extra subplot calculations ####

## Dominant height
# Function 1
AlturaDominante <- function(x, plotID = "PLOT_ID"){
  if(plotID %in% names(x)) {
    IDs = unique(x[[plotID]])
    Ho = rep(NA, length(IDs))
    names(Ho) = IDs
    for(i in 1:length(IDs)) {
      Ho[i] = .AlturaDominante_2(x$height[x[[plotID]] == IDs[i]],
                                 x$dbh[x[[plotID]] == IDs[i]],
                                 x$expan_subplot[x[[plotID]] == IDs[i]])
    }
    Hd <- data.frame(IDs, Ho)
    return(Hd)
  }
  return(.AlturaDominante_2(x$h, x$d, x$n))
}

# Function 2
.AlturaDominante_2 <- function(h, d, n){
  o <- order(d, decreasing=TRUE)
  h = h[o]
  n = n[o]
  ncum = 0 
  for(i in 1:length(h)){
    ncum = ncum + n[i]
    if(ncum>100) return(sum(h[1:i]*n[1:i], 
                            na.rm=TRUE)/sum(h[1:i]*n[1:i]/h[1:i], 
                                            na.rm=TRUE))
  }
  return(sum(h*n)/sum(n))
}


## Dominant diameter
# Function 1
DiametroDominante <- function(x, plotID = "PLOT_ID"){
  if(plotID %in% names(x)) {
    IDs = unique(x[[plotID]])
    Do = rep(NA, length(IDs))
    names(Do) = IDs
    for(i in 1:length(IDs)) {
      Do[i] = .DiametroDominante_2(x$d[x[[plotID]] == IDs[i]],
                                   x$dbh[x[[plotID]] == IDs[i]],
                                   x$expan_subplot[x[[plotID]] == IDs[i]])
    }
    Dd <- data.frame(IDs, Do)
    return(Dd)
  }
  return(.DiametroDominante_2(x$h, x$d, x$n))
}

# Function 2
.DiametroDominante_2 <- function(h, d, n){
  o <- order(d, decreasing=TRUE)
  d = d[o]
  n = n[o]
  ncum = 0 
  for(i in 1:length(d)){
    ncum = ncum + n[i]
    if(ncum>100) return(sum(d[1:i]*n[1:i], 
                            na.rm=TRUE)/sum(d[1:i]*n[1:i]/d[1:i], 
                                            na.rm=TRUE))
  }
  return(sum(d*n)/sum(n))
}

## extra calculations
calc_subplot <- function(subplot_stats, trees_in_subplot){

  # mean subplot values
  subplot_stats$DBHm <- subplot_stats$SUM_DBH/subplot_stats$N
  subplot_stats$DBHm_sp_1 <- subplot_stats$SUM_DBH_sp_1/subplot_stats$N_sp_1
  subplot_stats$DBHm_sp_2 <- subplot_stats$SUM_DBH_sp_2/subplot_stats$N_sp_2
  subplot_stats$DBHm_sp_3 <- subplot_stats$SUM_DBH_sp_3/subplot_stats$N_sp_3
  subplot_stats$Dg <- with(subplot_stats, 200*(G/N/pi)^0.5, na.rm=TRUE)
  subplot_stats$Dg_sp_1 <- with(subplot_stats, 200*(G_sp_1/N_sp_1/pi)^0.5, na.rm=TRUE)
  subplot_stats$Dg_sp_2 <- with(subplot_stats, 200*(G_sp_2/N_sp_2/pi)^0.5, na.rm=TRUE)
  subplot_stats$Dg_sp_3 <- with(subplot_stats, 200*(G_sp_3/N_sp_3/pi)^0.5, na.rm=TRUE)
  subplot_stats$Hm <- subplot_stats$SUM_H/subplot_stats$N
  subplot_stats$Hm_sp_1 <- subplot_stats$SUM_H_sp_1/subplot_stats$N_sp_1
  subplot_stats$Hm_sp_2 <- subplot_stats$SUM_H_sp_2/subplot_stats$N_sp_2
  subplot_stats$Hm_sp_3 <- subplot_stats$SUM_H_sp_3/subplot_stats$N_sp_3
  
  # Ho - avoiding trees from other species without height
  trees_in_subplot_picea <- trees_in_subplot[trees_in_subplot$species == 10, ]
  Ho <- AlturaDominante(as.data.frame(trees_in_subplot_picea), 'PLOT_ID_time')
  subplot_stats <- merge(subplot_stats, 
                         Ho,
                         by.x = 'PLOT_ID',  
                         by.y = 'IDs')  
  
  # Do
  Do <- DiametroDominante(as.data.frame(trees_in_subplot_picea), 'PLOT_ID_time')
  subplot_stats <- merge(subplot_stats, 
                         Do, 
                         by.x = 'PLOT_ID',  
                         by.y = 'IDs')  
  
  # slenderness
  subplot_stats$mean_slenderness <- subplot_stats$Hm*100/subplot_stats$DBHm  # normal slenderness
  subplot_stats$dom_slenderness <- subplot_stats$Ho*100/subplot_stats$Do  # dominant slenderness

  # Stand Density Index (SDI)
  r_value <- -1.664  # Pretzsch and Biber, 2005
  subplot_stats$SDI <- subplot_stats$N_ha*((25/subplot_stats$Dg)**r_value)
  
  # Hart Index (S)
  subplot_stats$S <- 10000/(subplot_stats$Ho*sqrt(subplot_stats$N_ha))  
  subplot_stats$S_staggered <- (10000/subplot_stats$Ho)*sqrt(2/subplot_stats$N_ha*sqrt(3))  
  
  # delete temp features
  subplot_stats <- select(subplot_stats, -c(SUM_DBH, SUM_DBH_sp_1, SUM_DBH_sp_2, SUM_DBH_sp_3, SUM_H, SUM_H_sp_1, SUM_H_sp_2, SUM_H_sp_3))
  
  return(subplot_stats)
}


#### tree size relationships ####

size_rels <- function(trees, subplot_stats){

  # merge datasets
  tree_plot_rel <- merge(trees, subplot_stats, by.x = 'TREE_ID_time', by.y = 'SUBPLOT_ID')
  
  # calculate variables
  tree_plot_rel$rel_dbh_Do <- tree_plot_rel$dbh / tree_plot_rel$Do
  tree_plot_rel$rel_dbh_Dg <- tree_plot_rel$dbh / tree_plot_rel$Dg
  tree_plot_rel$rel_h_Ho <- tree_plot_rel$height / tree_plot_rel$Ho
  tree_plot_rel$rel_h_Hm <- tree_plot_rel$height / tree_plot_rel$Hm
  tree_plot_rel <- select(tree_plot_rel, c(TREE_ID_time, rel_dbh_Do, rel_dbh_Dg, rel_h_Ho, rel_h_Hm))
  
  # join new and old datasets
  trees <- merge(trees, tree_plot_rel, by = 'TREE_ID_time')
  return(trees)
}