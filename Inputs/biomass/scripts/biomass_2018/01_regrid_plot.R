# Description -------------------------------------------------------------
#' Regrid 2018 GFED biomass burning emissions from 0.25 degree grid to municipalities
#' See first script: sa_biomass.py


# Library -----------------------------------------------------------------
library(sf)
library(tidyverse)
library(magrittr)
library(viridis)


# Directories -------------------------------------------------------------
data_year <- 2018
shp_dir <- paste0(getwd(), '/biomass/inputs/')
grid_dir <- paste0(getwd(), '/biomass/files/shapefile/')


sub_dir <- paste0(getwd(), '/biomass/files/biomass_', data_year, '/')
emis_dir <- paste0(sub_dir, 'emis_raw/')
export_dir <- paste0(sub_dir, 'emis_processed/')

plot_main <- paste0(getwd(), '/biomass/plots/biomass_', data_year, '/')
plot1_dir <- paste0(plot_main, 'emis_raw/')
plot2_dir <- paste0(plot_main, 'emis_regrid/')


# Inputs ------------------------------------------------------------------
grid_sf <- st_read(paste0(grid_dir, 'biomass_grid.shp')) %>% 
  select(-grid_area)

sa_sf <- st_read(paste0(shp_dir, 'southern_africa.shp'))  %>% 
  mutate(census_area = st_area(geometry))


poll_var <- c('PM2.5', 'NOx', 'NH3', 'OC', 'SO2', 'VOC', 'BC')
species <- c('PM25-PRI', 'NOX', 'NH3', 'OC', 'SO2', 'VOC', 'BC')

emis_tot <- data.frame(poll = poll_var,
                       inter_tot = NA,
                       regrid_tot = NA)

plot_emis <- function(sf_file, export_name, path_name) {
  a <- ggplot() +
    geom_sf(data = sf_file, aes(fill = emis_flux*1E6), color = NA, size = 0.1) + #1e6 tonnes/m2 to tonnes/km2
    scale_fill_viridis(trans = 'sqrt')  +
    labs(fill= 'emissions (tonnes/km2)') +
    theme_void() +
    theme(legend.title = element_text(size = 20)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.key.size = unit(1.5, 'cm')) +
    ggtitle(poll_var[poll_id])
  
  ggsave(filename = paste0(export_name, '.png'), 
         plot = a, path = path_name, height=7, width = 8.5, units = "in")
  
}


for (poll_id in 1:length(poll_var)) {
    emis_sf <- read_csv(paste0(emis_dir, poll_var[poll_id], '.csv')) %>% 
      inner_join(grid_sf, ., by = c('lon', 'lat')) %>% 
      select(-lon, -lat) %>% 
      mutate(emis_flux = emissions/grid_area)
    
    
    inter <- st_intersection(emis_sf, sa_sf) %>% 
      mutate(int_area = st_area(geometry))
    
    
    inter_info <- inter %>% 
      mutate(geometry = NULL,
             frac_of_grid = as.double(int_area/grid_area),
             frac_of_census = as.double(int_area/census_area)) %>% 
      as.data.frame()
    
    
    emis_census <- mutate(inter_info,
                          new_emis = emissions*frac_of_grid) %>% # area was already multiplied in sa_biomass.py
      group_by(census_id) %>% 
      summarise(emissions = sum(new_emis))
    
    
    final_sf <- left_join(sa_sf, emis_census) %>% 
      mutate(emis_flux = as.double(emissions/census_area))

    plot_emis(sf_file = emis_sf,
              export_name = poll_var[poll_id],
              path_name = plot1_dir)

    plot_emis(sf_file = final_sf,
              export_name = poll_var[poll_id],
              path_name = plot2_dir)
    
    emis_tot[poll_id, 'inter_tot'] <- sum(emis_sf$emissions)
    emis_tot[poll_id, 'regrid_tot'] <- sum(final_sf$emissions)
    
    emis_census %>% 
      rename(emis = emissions) %>% 
    write_csv(paste0(export_dir, species[poll_id], '.csv'))
  
}


emis_tot %<>%
  mutate(per_diff = 100*abs(regrid_tot - inter_tot)/(0.5*(regrid_tot + inter_tot)))




#note grid area in km2