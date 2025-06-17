# Description -------------------------------------------------------------
#' Aggregate VOC emissions 

# Library -----------------------------------------------------------------
library(tidyverse)
library(data.table)

# Directories -------------------------------------------------------------
sub_dir <- paste0(getwd(), '/biomass/files/')
emis_dir <- paste0(sub_dir, 'emis_raw/')

# Inputs ------------------------------------------------------------------
species <- c('NMHC', 'CH3OH', 'C2H5OH', 'CH2O', 'C2H4O', 'C3H6O',
             'HCOOH', 'CH3COOH', 'MEK', 'CH3COCHO', 'HOCH2CHO')

all_files <- paste0(emis_dir, species, '.csv')

voc_emis <- all_files %>% 
  {rbindlist(lapply(., fread))} %>% 
  group_by(lon, lat, grid_area) %>% 
  summarise(emissions = sum(emissions))

write_csv(voc_emis, paste0(emis_dir, 'VOC.csv'))
