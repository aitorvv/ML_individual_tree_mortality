#------------------------------------------------------------------------------------------#
####                              Dealing with climate data                             ####
#                                                                                          #
#                            Aitor Vázquez Veloso, 02/06/2023                              #
#------------------------------------------------------------------------------------------#

# That code uses the site coordinates to extract the climate data
# Later, climate variables are calculated

#### Initial setup ####

library(openxlsx)
library(rgdal)
library(raster)
library(sf)
library(SPEI)
library(plyr)
library(tidyverse)

setwd('ML_individual_tree_mortality/')

# coordinate points
my_points <- read.xlsx('1_data/0_raw/Koordinaten.xlsx', sep = ',')


#### Change coordinates format ####

# choose df and original CRS
coordinates(my_points) <- c("Longitude", "Latitude")
proj4string(my_points) <- CRS("+proj=longlat +datum=WGS84")  

# convert to UTM - not needed in our case
#locations_UTM <- spTransform(locations, CRS("+proj=utm +zone=32 ellps=WGS84"))

# For a SpatialPoints object rather than a SpatialPointsDataFrame, just do: 
#locations_UTM_lonlat <- as(locations_UTM, "SpatialPoints")


#### Extract T and P data: all variables in the same column ####

# path to folders of each experiment area
path <- "WorldClim/"
folder_list <- dir(path)

# main dataframes
clima <- tibble()
clima_tmp <- tibble()

for (folder in folder_list){
  
  # access to each file
  folder_path <- paste(path, folder, sep = '')
  files_list <- list.files(path = folder_path, pattern = ".tif")

  for (file in files_list){
    
    # get file variables by their name
    var <- substring(file, first = 12, last = 15)
    year <- substring(file, first = 17, last = 20)
    month <- substring(file, first = 22, last = 23)
    
    # read raster file
    raster_file <- raster(paste(folder_path, file, sep = '/'))

    # extract values 
    value <- extract(raster_file, my_points)
    
    # join information in a single df 
    clima_tmp_file <- tibble(as.tibble(my_points), var, year, month, value)
    clima_tmp <- rbind(clima_tmp, clima_tmp_file)
  }
  
  # join every df
  clima <- rbind(clima, clima_tmp)
}

rm(clima_tmp, clima_tmp_file, raster_file, folder, file, folder_list, folder_path,
   files_list, var, year, month, value, path, df)


#### Extract T and P data: variables splitted in columns ####

# path to folders of each experiment area
path <- "WorldClim/historical_monthly_data/"
folder_list <- dir(path)

# main dataframes
clima_tmp <- tibble()
prec <- tibble()
tmin <- tibble()
tmax <- tibble()

for (folder in folder_list){
  
  # access to each file
  folder_path <- paste(path, folder, sep = '')
  files_list <- list.files(path = folder_path, pattern = ".tif")
  
  for (file in files_list){
    
    # get file variables by their name
    var <- substring(file, first = 12, last = 15)
    year <- substring(file, first = 17, last = 20)
    month <- substring(file, first = 22, last = 23)
    
    # read raster file
    raster_file <- raster(paste(folder_path, file, sep = '/'))
    
    # extract values 
    value <- extract(raster_file, my_points)
    
    # join information in a single df 
    clima_tmp_file <- tibble(as.tibble(my_points), var, year, month, value)
    clima_tmp <- rbind(clima_tmp, clima_tmp_file)
  }
  
  # join every df
  if('prec' == substring(folder, 22, 25)){
    prec <- rbind(prec, clima_tmp)
  } else if('tmin' == substring(folder, 22, 25)){
    tmin <- rbind(tmin, clima_tmp)
  } else {
    tmax <- rbind(tmax, clima_tmp)
  }
  
  # delete tmp data
  clima_tmp <- tibble()
}

# rename columns
prec <- dplyr::rename(prec, prec = value)
tmin <- dplyr::rename(tmin, tmin = value)
tmax <- dplyr::rename(tmax, tmax = value)

# delete "var"
prec <- dplyr::select(prec, -var)
tmin <- dplyr::select(tmin, -var)
tmax <- dplyr::select(tmax, -var)

# join all variables on the same df
clima <- merge(prec, tmin, by = c('VF', 'Number', 'Longitude', 'Latitude', 'year', 'month'))
clima <- merge(clima, tmax, by = c('VF', 'Number', 'Longitude', 'Latitude', 'year', 'month'))

rm(clima_tmp, clima_tmp_file, raster_file, folder, file, folder_list, folder_path,
   files_list, var, year, month, value, path, prec, tmax, tmin)


#### Climate data curation ####

# ID and rename
clima$INVENTORY_ID <- substring(clima$VF, 1, 3)
clima <- dplyr::rename(clima,
                       site = Number)
clima <- dplyr::select(clima, INVENTORY_ID, site, Longitude, Latitude, 
                       year, month, prec, tmin, tmax)

# Mean temperature
clima$tmean <- (clima$tmax + clima$tmin)/2

# Mean year values
clima_year <- plyr::ddply(clima, c('INVENTORY_ID', 'site', 'Longitude', 'Latitude', 
                                   'year'), summarise, 
                          # CODES
                          INVENTORY_ID = unique(INVENTORY_ID),
                          site = unique(site), 
                          Longitude = unique(Longitude),
                          Latitude = unique(Latitude),
                          year = unique(year),
                          # variables
                          tmin = mean(tmin, na.rm = TRUE), 
                          tmax = mean(tmax, na.rm = TRUE),
                          tmean = mean(tmean, na.rm = TRUE),
                          prec = sum(prec, na.rm = TRUE)
)


#### Checkpoint ####

# save data
write.csv(clima, '1_data/tmp_DEN/2_clima/all_clima_worldclim_month.csv')
write.csv(clima_year, '1_data/tmp_DEN/2_clima/all_clima_worldclim_year.csv')

# load data
clima <- read.csv('1_data/tmp_DEN/2_clima/all_clima_worldclim_month.csv')
clima <- clima[-1]

trees <- read.csv('1_data/tmp_DEN/1_neighborhood/trees_r33.csv', sep = ',')
subplot_stats <- read.csv('1_data/tmp_DEN/1_neighborhood/subplot_stats_r33.csv', sep = ',')
neighborhood_stats <- read.csv('1_data/tmp_DEN/1_neighborhood/neighborhood_stats_r33.csv', sep = ',')

trees <- trees[-1]
subplot_stats <- subplot_stats[-1]
neighborhood_stats <- neighborhood_stats[-1]


#### Climate data indexes ####

# calculate climate indices
dat_clim <- dplyr::rename(clima, 
                   plot = site,
                   lat = Latitude,
                   lon = Longitude,
                   year = year,
                   month = month,
                   air_temperature_mean = tmean,
                   air_temperature_min = tmin,
                   air_temperature_max = tmax,
                   precipitation = prec)

# use the August SPEI integrated over 6 months (SPEI6, March to August)
dat_clim_indices <- purrr::map_df(
  .x = unique(dat_clim$plot),
  .f = ~{
    work_dat <- dat_clim |> 
      dplyr::filter(plot  == .x)
    
    work_dat_lat <- work_dat$lat[1]
    
    work_dat <- work_dat |>
      dplyr::mutate(pet = SPEI::hargreaves(Tmin = air_temperature_min,
                                           Tmax = air_temperature_max,
                                           Pre = precipitation,
                                           lat = work_dat_lat)) |> 
      dplyr::mutate(
        bal_1 = precipitation - pet
      ) # add climatic water balance
    
    spei3_1 <- spei(work_dat$bal_1, 3)
    spei6_1 <- spei(work_dat$bal_1, 6)
    spei12_1 <- spei(work_dat$bal_1, 12)
    
    work_dat_spei <- work_dat |>  
      dplyr::mutate(
        spei3_har = spei3_1$fitted,
        spei6_har = spei6_1$fitted,
        spei12_har = spei12_1$fitted
      )
    
    # deMartonne aridity index
    dm <- work_dat |> 
      dplyr::group_by(plot, year) |>
      dplyr::summarise(temp_mean = mean(air_temperature_mean),
                       prec_sum = sum(precipitation)) |>
      dplyr::ungroup() |>
      dplyr::mutate(dm = prec_sum / (temp_mean + 10))
    
    dplyr::full_join(work_dat_spei, dm, by = c("plot", "year")) |>
      dplyr::mutate(
        spei3_har = as.numeric(spei3_har),
        spei6_har = as.numeric(spei6_har),
        spei12_har = as.numeric(spei12_har)
      ) |> 
      dplyr::select(plot, lat, lon, year, month, 
                    air_temperature_mean, 
                    precipitation, 
                    spei3_har, spei6_har, spei12_har,
                    deMartonne_year = dm)
    
  })

# calculate annual means
dat_clim_indices_annual <- plyr::ddply(dat_clim_indices, c('year', 'plot'), summarise,
                                       M = sum(deMartonne_year/12, na.rm = TRUE),
                                       tmean = sum(air_temperature_mean/12, na.rm = TRUE),
                                       prec = sum(precipitation, na.rm = TRUE)
)

# get SPEI from August 
SPEI_aug <- dat_clim_indices[dat_clim_indices$month == 8, ]
SPEI_aug <- dplyr::select(SPEI_aug, plot, year, spei3_har, spei6_har, spei12_har)
SPEI_aug <- dplyr::rename(SPEI_aug,
                   'SPEI3_aug' = 'spei3_har',
                   'SPEI6_aug' = 'spei6_har',
                   'SPEI12_aug' = 'spei12_har')

# join it to annual means
dat_clim_indices_annual <- merge(dat_clim_indices_annual, SPEI_aug, by = c('plot', 'year'))

# check missing values
sapply(dat_clim_indices, function(x) sum(is.na(x))) # no missing values
sapply(dat_clim_indices_annual, function(x) sum(is.na(x))) # no missing values

# check variable types
str(dat_clim_indices)
str(dat_clim_indices_annual)

# export data
write.csv(dat_clim_indices, '1_data/tmp_DEN/2_clima/SPEI_M_monthly.csv', fileEncoding = 'UTF-8')
write.csv(dat_clim_indices_annual, '1_data/tmp_DEN/2_clima/SPEI_M_annual.csv', fileEncoding = 'UTF-8')


#### Grouping climate, tree and subplot data ####

trees <- merge(trees, dat_clim_indices_annual, by.x = c('site', 'year'), by.y = c('plot', 'year'))
final_df <- merge(trees, subplot_stats, by.x = c('TREE_ID_time', 'INVENTORY_ID', 'PLOT_ID_time'), by.y = c('SUBPLOT_ID', 'INVENTORY_ID', 'PLOT_ID'))
neighborhood_stats_tmp <- neighborhood_stats[neighborhood_stats$TREE_ID_time == neighborhood_stats$TREE_ID_center, ]
neighborhood_stats_tmp <- dplyr::select(neighborhood_stats_tmp, INVENTORY_ID, PLOT_ID_time, TREE_ID_time, bal_subplot, bal_subplot_ha)
final_df <- merge(final_df, neighborhood_stats_tmp, by = c('INVENTORY_ID', 'PLOT_ID_time', 'TREE_ID_time'))

#### Export data to statistics ####

write.csv(final_df, '1_data/tmp_DEN/2_clima/df_complete_r33.csv')
