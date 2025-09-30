# Description -------------------------------------------------------------
#' Save emission totals after setting each sector emissions to zero

  

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

data_year <- 2018
# Directories -------------------------------------------------------------
input_dir <- paste0(getwd(), '/inputs/emissions/sector_emissions/')
edgar_dir <- paste0(input_dir, 'EDGAR/')
biomass_dir <- paste0(input_dir, 'biomass/files/biomass_', data_year, '/emis_processed/')
biogenic_dir <- paste0(input_dir, 'biogenics/emis_processed/') #ONLY FOR VOC GROUND
export_dir <- paste0(getwd(), '/inputs/emissions/sectors_zeroed/')
# information -------------------------------------------------------------
#' EDGAR doesn't assign stack height for each sector, the designation of ground
#' vs elevated is assumed for each sector. Refine if needed
sector_info <- read_excel(paste0(input_dir, 'edgar_sectors.xlsx')) %>% 
  select(-poll,-sector)


poll_var <- c("SO2", "NOX", "NH3", "VOC", "OC", "PM25-PRI", "BC")


edgar_emis <- list.files(edgar_dir, full.names = T) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) 

edgar_emis %<>% 
  left_join(sector_info, by = 'code') %>% 
  select(sector = code, type = sector_type, census_id, poll, emis) %>% 
  filter(sector != 'AWB')  #agricultural waste burning is accounted for in GFED, remove from EDGAR

biomass <- list.files(biomass_dir, full.names = T, pattern = '.csv') %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  mutate(sector = 'Biomass',
         type = 'ground')


biogenic_VOC <- read_csv(paste0(biogenic_dir, 'biogenic_VOC.csv')) %>% 
  mutate(poll = 'VOC',
         sector = 'Biogenic',
         type = 'ground'
         )


all_emis <- rbind(edgar_emis, biomass, biogenic_VOC) %>% 
            mutate(poll = case_when(poll == 'NOx' ~ 'NOX',
                            poll == 'NMVOC' ~ 'VOC',
                            poll == 'PM2.5' ~ 'PM25-PRI',
                            TRUE ~ poll))

all_sectors <- unique(all_emis$sector)

#Loop to create folders
for (sec in 1:length(all_sectors)) {
  sector_dir <- paste0(export_dir, all_sectors[sec], '/')
  
  if (!dir.exists(sector_dir)) {   # Check so it doesn't fail if folder already exists
    dir.create(sector_dir)
  }
}


get_emis <- function(height) {
 poll_emis <- emis_df %>% 
  filter(poll == poll_var[k] & type == height) %>% 
    group_by(poll, type, census_id) %>% 
    summarise(emis = sum(emis))
 
 write_csv(poll_emis, file = paste0(sector_path, poll_var[k], '_', height, '.csv'))
    
}

for (j in 1:length(all_sectors)) {
  
  emis_df <- all_emis %>% 
    mutate(emis = ifelse(sector == all_sectors[j], 0, emis)) # zero out sector emissions
  
  sector_path <-paste0(export_dir, all_sectors[j], '/') 
  
  for (k in 1:length(poll_var)) {
    
    
    get_emis('ground')
    
    get_emis('elevated')
    
    
  
    }
  
  
}

