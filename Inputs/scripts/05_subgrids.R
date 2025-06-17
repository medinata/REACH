# Description -------------------------------------------------------------
#' 6/28/22: Divide each source geography into smaller divisions/sub-grids. 
#' Overlay a rectangular box over the source geography based on the extent.
#' Then, divide the region into an equal number of grid cells. Test out a few
#' sub grid sizes to determine the optimal size
#' 

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)


# Directories -------------------------------------------------------------
open_dir <- function(name_ext) {
  paste0(getwd(),'/', name_ext, '/')
}

input_dir <- open_dir('inputs')
sf_dir <- paste0(getwd(), '/inputs/shapefiles/')
export_dir <- open_dir('misc/sub_grids')

# Main code ---------------------------------------------------------------
div_sf <- st_read(paste0(sf_dir, 'southern_africa.shp'))

div_df <- mutate(div_sf, geometry = NULL) %>%
  as.data.frame()


#' Number of grid cells in x and y direction to overlay over extent of census 
#' geography
grid_num <- 10
grid_centroid_list <- list()


for (i in 1:nrow(div_sf)) {
  # Create grid from sf object
  grid_coord <- st_make_grid(div_sf[i,"geometry"], n = c(grid_num, grid_num), what = "polygons")
  
  # Compute centroids of each grid cell
  grid_centroid <- st_centroid(grid_coord) %>%
    {do.call(rbind, .)} %>%
    as.data.frame() %>%
    setNames(c("lon", "lat")) %>% 
    mutate(., census_id = div_df[i,"census_id"])
  
  grid_centroid_list[[i]] <- grid_centroid
  
  
}

grid_info <- data.table::rbindlist(grid_centroid_list)

write_csv(grid_info, paste0(input_dir, 'sub_grids.csv'))



# plot grid cells for a census geography ----------------------------------
#' Visualize how a uniform grid is overlayed over the extent of a census
#' geography
div_id <- div_sf$census_id[1332]


geo_grid <- filter(div_sf, census_id == div_id) %>%
  st_make_grid(n = c(grid_num, grid_num), what = "polygons")

centroid <- filter(grid_info, census_id == div_id)


ggplot() +
  geom_sf(data = filter(div_sf, census_id == div_id), fill = NA) +
  geom_sf(data = geo_grid, color = "black", fill = NA) +
  geom_point(data = centroid, aes(lon, lat),
             color = "red", size = 0.5) +
  theme_void()












