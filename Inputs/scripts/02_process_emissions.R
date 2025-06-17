# Description -------------------------------------------------------------
#' Re-grid EDGAR emissions from 0.1 x 0.1 spatial resolution into
#' local geography resolution. This process uses area emission-weighted averaging to
#' allocate emissions from the EDGAR grid cell to each local geography location. 
#' This script uses Southern Africa as an example.
#' The post-processed emissions are in units of tonnes/year


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(raster)
library(stars)


# Directories -------------------------------------------------------------
input_dir <- paste0(getwd(), '/files/')
sf_dir <- paste0(input_dir, 'shapefiles/')
emis_dir <- paste0(input_dir, 'emissions/nc_files/')
info_dir <- paste0(input_dir, 'emissions/info/')
export_dir <- paste0(input_dir, 'emissions/emission_files/')


# Change shapefile to your domain -----------------------------------------
#' Shapefiles for Southern Africa
sf_file <- st_read(paste0(sf_dir, 'southern_africa.shp')) %>% 
  mutate(census_area = st_area(geometry))

geo_df <- sf_file %>% 
  mutate(geometry = NULL) %>% 
  as.data.frame()

# information -------------------------------------------------------------
proj_code <- st_crs(4326) 


#' This file was created based on the edgar version listed for the pollutants
#' of interest. Check and adjust sector and pollutant names for your purpose
sector_info <- read_csv(paste0(info_dir, 'edgar_sectors.csv'))



# Re-grid emissions to census geographies ---------------------------------
all_emis <- data.frame(emis_file = list.files(path = emis_dir)) %>% 
  mutate(code = gsub(".*2018_\\s*|.0.1x0.1.nc.*", "", emis_file),
         export_name = gsub("EDGARv6.1_*|.0.1x0.1.nc.*", "", emis_file),
         poll = gsub("EDGARv6.1_*|_2018.*", "", emis_file))

emis_summary <- mutate(all_emis,
                       emis_tot = NA)


for(j in 1:nrow(all_emis)) {
  emis_raster <- raster(paste0(emis_dir, all_emis$emis_file[j])) 
  
  #' Crop out a rectangular grid using the extent of the Southern Africa shapefile
  #' NOTE: This includes grid cells outside the Southern African boundaries,
  #' so the emissions before and after regridding are going to be different. This
  #' is done to ensure that every grid cell intersecting SA is accounted for. 
  
  domain_raster <- crop(emis_raster, sf_file) 
  
  #' Convert raster into stars object (array) before converting into sf object
  emis_sf <- st_as_stars(domain_raster, crs = proj_code) %>% 
    st_as_sf(crs = proj_code) %>% 
    rename(emis_rate = 1) %>% #kg/m2/s
    mutate(emis_grid_area = unclass(st_area(geometry)),
           emis = emis_rate*emis_grid_area*3.154E7/1000) #kg/m2/s to tonnes/year
  
  #' Determine intersections between 0.1 degree grid cells and census 
  #' divisions
  inter <- st_intersection(emis_sf, sf_file) %>% 
    mutate(int_area = st_area(geometry)) 
  
 #' Determine fraction of EDGAR grid cell that intersects each local geography
  inter_info <- inter %>% 
    mutate(geometry = NULL,
           frac_of_grid = as.double(int_area/emis_grid_area)) %>% 
    as.data.frame()
  
  #' Compute area-weighted average emissions
  emis_census <- mutate(inter_info,
                        new_emis = emis*frac_of_grid) %>% 
    group_by(census_id) %>% 
    summarise(emis = sum(new_emis)) %>% 
    left_join(dplyr::select(geo_df, -census_area), by = "census_id") %>% 
    mutate(code = all_emis$code[j],
           poll = all_emis$poll[j])
  
  write_csv(emis_census, paste0(export_dir, all_emis$export_name[j],'.csv'))
  
  
  emis_summary[j,'emis_tot'] <- sum(emis_census$emis)
  
  # progress tracker
  if (j %% 100 == 0) {
    print(j/nrow(all_emis)*100)
  }

  
}



write_csv(emis_summary, paste0(info_dir, 'regrided_emis.csv'))


test_df <- emis_sf %>% 
  mutate(geometry = NULL) %>% 
  as.data.frame()





#plot(domain_raster)
  
  


# compare emissions -------------------------------------------------------
old_dir <- paste0(input_dir, 'old_edgar/info/')

old_emis <- read_csv(paste0(old_dir, 'regrided_emis.csv')) %>% 
  dplyr::select(export_name, old = emis_tot)

compare_emis <- left_join(old_emis, emis_summary) %>% 
  mutate(diff = emis_tot - old)







