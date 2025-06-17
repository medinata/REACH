# Description -------------------------------------------------------------
#' This script applies emissions to the source-receptor matrices to estimate concentrations 
#' of sulfate, total (gas + particulate) nitrate, total (gas + particulate) ammonia, 
#' primary PM2.5, secondary organic carbon and primary organic carbon at all receptor locations


# Variables ---------------------------------------------------------------
#' All units are ug/m3 
#' OC_tot: primary plus secondary organic carbon
#' OC_primary: primary organic carbon
#' OC_secondary: secondary organic carbon
#' SO4: sulfate
#' Tot_HNO3: total nitrate (particulate NO3- plus gas HNO3)
#' Tot_NH3: total ammonia (particulate NH4+ plus gas NH3)
#' PM: primary PM2.5
#' OA_to_OC: organic aerosol to organic carbon ratio


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)



# Directories -------------------------------------------------------------
curr_dir <- getwd()
result_file <- paste0(curr_dir,'/results/matrices/SR_' )
input_dir <- paste0(curr_dir,'/inputs/')
emis_dir <- paste0(input_dir,'emissions/processed_emissions/')
export_dir <- paste0(curr_dir, '/results/concentrations/')

Divisions <- read_csv(paste0(input_dir,"southern_africa.csv")) %>%
  as.data.frame()


species <- c("SO4","Tot_HNO3","Tot_NH3","OC_secondary","OC_primary","PM", "BC")

tot_conc <- data.frame(matrix(0, ncol = length(species), nrow = nrow(Divisions)))
colnames(tot_conc) <- species

# calibration coefficients -------------------------------------------------

poll_info <- c("SO2", "NOX", "NH3", "VOC", "OC", "PM25-PRI", "BC")


OA_to_OC <- 1.8


open_matrix <- function(name_ext, poll) {
  matrix_path <- paste0(result_file,poll,'_')
  fread(paste0(matrix_path,name_ext,'.csv'))
}


read_matrix <- function(poll_name) {
  elevated_poll <- open_matrix("elevated", poll_name)
  ground_poll <- open_matrix("ground", poll_name)
  
  rbindlist(list(elevated_poll, ground_poll)) %>% 
    as.matrix()
  
  
}

open_emis <- function(height) {
  read_csv(paste0(emis_dir, poll_info[j], '_', height, '.csv'))
}


for (j in 1:length(poll_info)) {
    
    #'Source-receptor matrix for primary organics is the same as primary PM2.5.
    #'It is converted from primary organic aerosol into primary organics at the end
    #'of the script.
    if(poll_info[j] %in% c('OC', 'BC')) {
      sr_matrix <- read_matrix('PM25-PRI')
      
    }  
    
    #' Source-receptor matrix for other pollutants
    if (!poll_info[j] %in% c('OC', 'BC')) {
      sr_matrix <- read_matrix(poll_info[j])
    }

  # emissions matrix (metric tons)
  elevated_emis <- open_emis('elevated')
  
  ground_emis <- open_emis('ground')
    
  # emissions vector
  tot_emis <- rbind(elevated_emis, ground_emis) %>% 
    {as.matrix(.$emis, ncol = 1)}
  
  poll_tot <- list()
  
  # compute baseline pollutant concentrations (ug/m3)
  for (i in 1:ncol(sr_matrix)) {
    poll_tot[[i]] <-  sr_matrix[,i] %*% tot_emis
  }
  
  # receptor concentrations in ug/m3
  tot_conc[,j] <- do.call(rbind, poll_tot)
  
}

final_conc <- tot_conc %>% 
  mutate(census_id = Divisions$census_id, # location geography id
         OC_primary = OC_primary/OA_to_OC) %>%  # convert primary organic aerosol into primary organic carbon since primary PM2.5 matrix was used) 
  relocate(census_id, .before = SO4)

# save concentrations prior to calibration
write_csv(final_conc, paste0(export_dir, 'uncalibrated_conc.csv'))













