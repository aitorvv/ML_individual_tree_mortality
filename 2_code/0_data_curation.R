#------------------------------------------------------------------------------------------#
####                 Data adaptation to test dataframe to reuse code                    ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 31/05/2023                              #
#                              Last modification: 19/06/2023                               #
#------------------------------------------------------------------------------------------#

# To reduce the time coding, I decided to adapt that new dataset to the previous test
# dataset to skip code modifications

#### Initial setup ####

library(tidyverse)
library(readxl)
library(openxlsx)
library(beepr)

setwd('ML_individual_tree_mortality/')

# path to folders of each experiment area
path <- "ML_individual_tree_mortality/1_data/0_raw/VF_daten/"
folder_list <- dir(path)


#### Join all the information in a single df ####

# main dataframe
df <- tibble()

for (folder in folder_list){
  
  # access to each file
  folder_path <- paste(path, folder, sep = '')
  files_list <- list.files(path = folder_path, pattern = "xls")
  df_tmp <- tibble()
  
  for (file in files_list){
    
    # get inventory name
    inv_name <- substring(folder, first = 1, last = 3)
    
    # read file and delete first row
    xls_file <- readxl::read_excel(paste(folder_path, file, sep = '/'), sheet = 'Tabelle1', skip = 1)

    # extract general variables and add inventory name
    general <- select(xls_file, c("VNR", "P", "BNR", "BA", "x", "y", "z"))
    general <- cbind(inv_name, general)
    general <- dplyr::rename(general, 
                             INVENTORY_ID = inv_name,
                             site = VNR,
                             plot = P,
                             Nr = BNR,
                             Artcode = BA)
    
    # extract variables measured several times
    dbh <- xls_file  %>% select(starts_with("D_"))
    height <- xls_file  %>% select(starts_with("H_"))
    hcb <- xls_file  %>% select(starts_with("KRA_"))
    removed <- xls_file  %>% select(starts_with("ka_"))
    observation <- xls_file  %>% select(starts_with("Bem_"))
    reason <- xls_file  %>% select(starts_with("Bken_"))
    
    # extract years and adapt to a dataframe
    year <- as.data.frame(parse_number(colnames(dbh)))
    colnames(year) <- c("year")
    n_years <- nrow(year)
    year <- year[rep(seq_len(nrow(year)), each = nrow(xls_file)), ]
    year <- as.data.frame(year)
    
    # unlist each variable
    dbh <- data.frame(dbh = unlist(dbh, use.names = FALSE))
    height <- data.frame(height = unlist(height, use.names = FALSE))
    hcb <- data.frame(hcb = unlist(hcb, use.names = FALSE))
    removed <- data.frame(removed = unlist(removed, use.names = FALSE))
    observation <- data.frame(observation = unlist(observation, use.names = FALSE))
    reason <- data.frame(reason = unlist(reason, use.names = FALSE))
    
    # adapt general information (structure is different than year)
    general_final <- tibble()
    for (measurement in 1:n_years){
      general_final <- rbind(general_final, general)  
    }
    
    # create a dataframe with all the information
    df_tmp_file <- tibble(general_final, year, dbh, height, hcb, removed, reason, observation)
    df_tmp <- rbind(df_tmp, df_tmp_file)
  }
  
  # add temporal df to the main one
  df <- rbind(df, df_tmp)
}

rm(dbh, df_tmp, general, general_final, height, hcb, observation, reason, removed, 
   xls_file, year, file, files_list, folder, folder_list, folder_path, inv_name,
   measurement, n_years, path, df_tmp_file)


#### IDs, units and coordinates ####

# calculate IDs
df$PLOT_ID <- paste(df$site, df$plot, sep = '_')
df$PLOT_ID_time <- paste(df$PLOT_ID, '_', df$year, sep = '')
df$TREE_ID <- paste(df$PLOT_ID, df$Nr, sep = '_')
df$TREE_ID_time <- paste(df$TREE_ID, '_', df$year, sep = '')

# units
df$dbh <- df$dbh/10 # mm to cm
df$height <- df$height/10 # dm to m
df$hcb <- df$hcb/10 # dm to m

# correct coordinates
df$x <- ifelse(df$x < 0, -df$x, df$x)

# input missing coordinate values - not possible
#coord <- df_clean[!is.na(df_clean$x), ]
#no_coord <- df_clean[is.na(df_clean$x), ]
#no_coord$TREE_ID %in% coord

# clean trees under inventariable dbh
df <- df[df$dbh >= 5, ]

# clean trees dead on the previous measurement
df <- df[!is.na(df$dbh), ]

# clean trees without coordinates
df <- df[!is.na(df$x), ]


#### Adapt mortality variables ####

# new columns to disting between natural mortality and thinning
df$thinned <- ifelse(substr(tolower(df$reason), 1, 3) %in% 'bad' & df$removed == 1, 1, 0)
df$dead <- ifelse(!(substr(tolower(df$reason), 1, 3) %in% 'bad') & df$removed == 1, 1, 0)


#### Input missing height values ####

## firstly, heights are calculated using the trees for the same inventory and year of measurement
df_full_height <- tibble()

for (plot in unique(df$PLOT_ID)){
  
  # filter data
  plot <- df[df$PLOT_ID == plot, ]
  
  for (year in unique(plot$year)){
    
    # select data
    plot_year <- plot[plot$year == year, ]
    trees_with_h <- plot_year[!is.na(plot_year$height), ]
    trees_without_h <- plot_year[is.na(plot_year$height), ]
    trees_without_h_no_picea <- trees_without_h[trees_without_h$Artcode != 10, ]
    trees_without_h_picea <- trees_without_h[trees_without_h$Artcode == 10, ]
    
    # skip no data cases
    if(nrow(trees_with_h) < 5){
            # add all the trees to the new df
            df_full_height <- rbind(df_full_height, trees_with_h, trees_without_h)
    } else {
            # create a log model
            hd_model <- lm(trees_with_h$height ~ log(trees_with_h$dbh))
            
            # predict height values
            trees_without_h_picea$height <- hd_model$coefficients[1] + hd_model$coefficients[2]*log(trees_without_h_picea$dbh)
            
            # add all the trees to the new df
            df_full_height <- rbind(df_full_height, trees_with_h, trees_without_h_picea, trees_without_h_no_picea)
    } # if statement
  } # year for loop
}

## secondly, trees with missing values for height are calculated by using all the trees 
#  with height value on the same inventory
df_full_height_2 <- tibble()

for (plot in unique(df_full_height$PLOT_ID)){
  
  # filter data
  plot_inv <- df_full_height[df_full_height$PLOT_ID == plot, ]
  
  # separate data
  trees_with_h_2 <- plot_inv[!is.na(plot_inv$height), ]
  trees_without_h_2 <- plot_inv[is.na(plot_inv$height), ]
  trees_without_h_2_no_picea <- trees_without_h_2[trees_without_h_2$Artcode != 10, ]
  trees_without_h_2_picea <- trees_without_h_2[trees_without_h_2$Artcode == 10, ]
  
  # fit the model
  main_hd_model <- lm(trees_with_h_2$height ~ log(trees_with_h_2$dbh))
  
  # predict height values
  trees_without_h_2_picea$height <- main_hd_model$coefficients[1] + main_hd_model$coefficients[2]*log(trees_without_h_2_picea$dbh)
  
  # add all the trees to the new df
  df_full_height_2 <- rbind(df_full_height_2, trees_with_h_2, trees_without_h_2_picea, trees_without_h_2_no_picea)
}

# clean trees with lowest height due to predictions
df_full_height_2 <- df_full_height_2[df_full_height_2$height > 2, ]
df_full_height_2 <- df_full_height_2[!is.na(df_full_height_2$height), ]

rm(df, df_full_height, main_hd_model, hd_model, plot, plot_year, trees_with_h, 
   trees_with_h_2, trees_without_h, trees_without_h_2, year, plot_inv, 
   trees_without_h_2_picea, trees_without_h_2_no_picea, trees_without_h_no_picea, trees_without_h_picea)


#### Include stand age ####

# import dataset
yield_tables <- read.xlsx("1_data/0_raw/Fi-Daten__age.xlsx", 1, startRow = 1)

# clean measurement year
yield_tables$year <- substr(yield_tables$JAHR, 1, 4)
yield_tables$year <- ifelse(yield_tables$year == '994F', 1994, yield_tables$year)
yield_tables$year <- as.numeric(yield_tables$year)

# extract variables and merge datasets
yield_tables <- select(yield_tables, VNR, Bezeichnung, Parz, year, A)
yield_tables <- dplyr::rename(yield_tables, 
                              site = VNR, 
                              INVENTORY_ID = Bezeichnung, 
                              plot = Parz, 
                              AGE = A)
yield_tables$PLOT_ID_time <- paste(yield_tables$site, yield_tables$plot, 
                                   yield_tables$year, sep = '_')
yield_tables <- select(yield_tables, PLOT_ID_time, AGE)

# for some reason I don't understand that merge is not working properly...
# I found that the years for which I have age doesn't match exactly with the year
# the measurement was taken, so I will introduce them manually
yield_tables_empty <- tibble(.rows = 24)
yield_tables_empty$PLOT_ID_time <- c("603_1_1980", "603_2_1980", "603_3_1980", "603_4_1980", 
                                     "603_5_1980", "603_6_1980", "604_1_1980", "604_2_1980", 
                                     "604_3_1980", "604_4_1980", "605_1_1976", "605_2_1976",
                                     "605_2_2015", "605_3_1976", "605_3_2015", "605_4_1976", 
                                     "605_4_2015", "605_5_1976", "605_5_2015", "605_6_1976", 
                                     "605_6_2015", "605_7_2015", "605_8_2015", "622_1_1994")
yield_tables_empty$AGE <- c(17, 17, 17, 17, 
                            17, 17, 17, 17, 
                            17, 17, 24, 24,
                            63, 24, 63, 24,
                            63, 24, 63, 24, 
                            63, 63, 63, 20)
yield_tables <- rbind(yield_tables, yield_tables_empty)
yield_tables <- yield_tables[!duplicated(yield_tables$PLOT_ID_time), ]

# merge data
df <- merge(yield_tables, df_full_height_2, by = 'PLOT_ID_time', 
            all.y = TRUE)

# clean data
df$year <- as.numeric(df$year)
df <- df[!is.na(df$Nr), ]
df <- df[!duplicated(df$TREE_ID_time), ]
rm(df_full_height_2, yield_tables, yield_tables_empty)
  

#### Export results ####

write.csv(df, '1_data/tmp_DEN/0_initial_df_clean/initial_df_clean.csv', fileEncoding = 'UTF-8')

# make some sound to indicate the finish
beep(sound = 8, expr = NULL)
