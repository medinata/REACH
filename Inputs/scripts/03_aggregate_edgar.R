# Description -------------------------------------------------------------
# Aggregate emissions based on source type (elevated vs ground)

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Directories -------------------------------------------------------------
input_dir <- paste0(getwd(), '/files/emissions/')
info_dir <- paste0(input_dir, 'info/')
emis_dir <- paste0(input_dir, 'emission_files/')
emis_ncdir <- paste0(input_dir, 'nc_files/')
export_dir <- paste0(input_dir, 'edgar_totals/')

# information -------------------------------------------------------------
#' EDGAR doesn't assign stack height for each sector, the designation of ground
#' and elevated sources is assumed for each sector. Refine if needed
sector_info <- read_csv(paste0(info_dir, 'edgar_sectors.csv'))

data_year <- '2018'


all_emis <- data.frame(emis_file = list.files(path = emis_dir)) %>% 
  mutate(code = gsub(paste0('.*', data_year,'_\\s*|.csv'), "", emis_file),
         poll = str_extract(emis_file, "^[^_]+") ,
         poll = case_when(poll == 'NOx' ~ 'NOX',
                          poll == 'NMVOC' ~ 'VOC',
                          poll == 'PM2.5' ~ 'PM25-PRI',
                          TRUE ~ poll)) %>% 
  left_join(dplyr::select(sector_info, code, sector_type), by = 'code')



poll_var <- unique(all_emis$poll)

#' The average stack height for South Africa country plants are ~220 m. 
#' Assume effective stack heights of 220 m for all elevated point sources 
#' in the South African region for baseline concentrations
elevated_height <- 220
ground_height <- 10


#' Aggregate emissions by sector type 
get_emission <- function(sector_height) {
  filter(poll_files, sector_type == sector_height) %>% 
    {.$emis_file} %>% 
    paste0(emis_dir, .) %>% 
    lapply(read_csv) %>% 
    do.call("rbind", .) %>% 
    group_by(census_id, geo_name, lon, lat, country, poll) %>% 
    summarise(emis = sum(emis))
  
}

export_emis <- function(emis_file, sector_height) {
  write_csv(emis_file, paste0(export_dir, poll_var[j],'_', sector_height,'.csv'))
}


for (j in 1:length(poll_var)) {
  poll_files <- filter(all_emis, poll == poll_var[j])
  
  
  get_emission('ground') %>% 
    mutate(eff_height = ground_height) %>% 
    export_emis(emis_file = ., sector_height = 'ground')
  
  get_emission('elevated') %>% 
    mutate(eff_height = elevated_height) %>% 
    export_emis(emis_file = ., sector_height = 'elevated')
  
}


