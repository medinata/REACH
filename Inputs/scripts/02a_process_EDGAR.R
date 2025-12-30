# Description -------------------------------------------------------------
#' Save intersections between EDGAR grid and census geographies



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

#' sf_use_S2(FALSE) assumes planar geometry. Differences between sf_use_s2(FALSE)
#' and sf_use_s2(TRUE) are small. Read more about differences here:
#' https://cran.r-project.org/web/packages/sf/vignettes/sf7.html
sf::sf_use_s2(FALSE)

domain_sf <- st_read(paste0(sf_dir, 'southern_africa.shp')) %>% 
  mutate(census_area = st_area(geometry))

domain_df <- domain_sf %>% 
  mutate(geometry = NULL) %>% 
  as.data.frame()

# information -------------------------------------------------------------
proj_code <- st_crs(4326) 

sector_info <- read_csv(paste0(info_dir, 'edgar_sectors.csv'))



# Re-grid emissions to census geographies ---------------------------------
all_emis <- data.frame(emis_file = list.files(path = emis_dir)) %>% 
  mutate(code = gsub(".*_2022_\\s*|_flx.nc.*", "", emis_file),
         poll = gsub("v8.1_FT2022_AP_*|_2022.*", "", emis_file),
         export_name = paste0(poll, '_', code, '_2022'))

emis_summary <- mutate(all_emis,
                       emis_tot = NA)



j <- 1


emis_raster <- raster(paste0(emis_dir, all_emis$emis_file[j])) 

#' Crop out a rectangular grid using the extent of the region shapefile
#' NOTE: This includes grid cells outside the region boundaries,
#' so the emissions before and after regridding are going to be different. This
#' is done to ensure that every grid cell intersecting the region is accounted for. 

# Add a 50 km buffer around the domain
pad_lon <- 0.5  # ~50 km at equator
pad_lat <- 0.5  # ~50 km

bb <- st_bbox(domain_sf)

bb_exp <- bb
bb_exp["xmin"] <- bb["xmin"] - pad_lon
bb_exp["xmax"] <- bb["xmax"] + pad_lon
bb_exp["ymin"] <- bb["ymin"] - pad_lat
bb_exp["ymax"] <- bb["ymax"] + pad_lat

domain_raster <- crop(emis_raster, bb_exp) 

#' Convert raster into stars object (array) before converting into sf object
grid_sf <- st_as_stars(domain_raster, crs = proj_code) %>% 
  st_as_sf(crs = proj_code) %>% 
  rename(emis_rate = 1) %>% #kg/m2/s
  mutate(emis_grid_area = unclass(st_area(geometry)),
         emis = emis_rate*emis_grid_area*3.154E7/1000) #kg/m2/s to tons/year

  
# Assign ID to grid_sf
n <- nrow(grid_sf)
w <- nchar(n)

grid_sf$grid_id <- sprintf(paste0("g%0", w, "d"), seq_len(n))


grid_df <- grid_sf %>% 
  mutate(geometry = NULL) %>% 
  as.data.frame()

inter <- st_intersection(grid_sf["grid_id"], domain_sf["census_id"]) %>% 
  mutate(int_area = st_area(geometry)) 

#' Determine intersections between 0.1 degree grid cells and census 
#' divisions

inter_info <- inter %>% 
  mutate(geometry = NULL) %>% 
  as.data.frame() %>% 
  left_join(grid_df, by = 'grid_id') %>% 
  mutate(frac_of_grid = as.double(int_area/emis_grid_area)) %>% 
  dplyr::select(grid_id, census_id, frac_of_grid)
  

write_csv(inter_info, paste0(info_dir, 'inter_info.csv'))


