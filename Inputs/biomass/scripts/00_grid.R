# Description -------------------------------------------------------------




# Libraries ---------------------------------------------------------------
library(raster)
library(sf)
library(tidyverse)
library(magrittr)
library(viridis)


# Directories -------------------------------------------------------------
sub_dir <- paste0(getwd(), '/biomass/files/')
emis_dir <- paste0(sub_dir, 'emis_raw/')
shp_dir <- paste0(getwd(), '/biomass/inputs/')
export_dir <- paste0(sub_dir, 'shapefile/')

# x_min <- -180
# x_max <- 180
# y_min <- -90
# y_max <- 90
# 
# coord_ref <- st_crs(4326)
# 
# test_grid <- raster(nrow = 720,
#        ncol = 1440,
#        xmn = x_min,
#        xmx = x_max,
#        ymn = y_min,
#        ymx = y_max,
#        crs = coord_ref)
# 
# grid_sf <- st_as_sf(test_grid)


div_sf <- st_make_grid(cellsize = 0.25,
                          offset = c(-180,-90),
                          n = c(1440, 720),
                          crs = st_crs(4326),
                          what = 'polygons') %>% 
  st_sf()


grid_cent <- st_centroid(div_sf) %>% 
  st_coordinates() %>% 
  {tibble(lon = .[ , 1],
          lat = .[ , 2])} 

div_sf %<>% 
  dplyr::bind_cols(grid_cent) %>% 
  relocate(c(lon, lat), .after = 1)


sa_sf <- st_read(paste0(shp_dir, 'southern_africa.shp'))

# filter out gfed southern africa region (SHAP) from global domain
emis_test <- read_csv(paste0(emis_dir, 'OC.csv')) %>% 
  inner_join(div_sf, ., by = c('lon', 'lat'))

# filter out REACH southern africa domain  
ind_inter <- unlist(st_intersects(sa_sf$geometry, emis_test$geometry)) %>% 
  unique()

final_sf <- emis_test[ind_inter,] %>% 
  select(-emissions) %>% 
  mutate(grid_area = grid_area/1E6) # area in km2

st_write(final_sf, paste0(export_dir, 'biomass_grid.shp'))



#final_sf <- st_intersection(emis_test, sa_sf[,c('geometry')])

# final_sf2 <- emis_test %>% 
#              filter(lon %in% (11.6:40.9)) 

# final_sf2 <- emis_test %>% 
#   filter(between(lon, 10, 42)) %>% 
#   filter(between(lat, -36, -3))

#final_sf3 <- emis_test[which(unlist(st_intersects(sa_sf$geometry, emis_test$geometry)) == 1)]


# range(emis_test$lat)
# 
# 
# 
# ggplot() +
#   geom_sf(data = emis_test, fill = NA) +
#   geom_point(data = emis_test, aes(lon, lat), shape = 23, 
#              color = "red", size = 0.5)


ggplot() +
  geom_sf(data = final_sf, aes(fill = emissions), color = NA, size = 0.1) +
  geom_sf(data = sa_sf, fill = NA) +
  scale_fill_viridis(trans = 'sqrt')  +
  labs(fill= 'emissions (tonnes)') +
  theme_void() +
  theme(legend.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.key.size = unit(1.5, 'cm')) #+
  #ggtitle(fill_name)



# re-grid emissions -------------------------------------------------------


