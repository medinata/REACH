
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)



# Directories -------------------------------------------------------------
curr_dir <- getwd()
result_file <- paste0(curr_dir,'/results/matrices/SR_' )
input_dir <- paste0(curr_dir,'/inputs/')
emis_main <- paste0(input_dir, 'emissions/sectors_zeroed/')
baseconc_dir <- paste0(curr_dir, '/results/concentrations/')
script_dir <- paste0(curr_dir, '/scripts/concentrations/')
export_dir1 <- paste0(curr_dir, '/results/sector_conc/')
export_dir2 <- paste0(curr_dir, '/results/sector_deaths/')

Divisions <- read_csv(paste0(input_dir,"southern_africa.csv")) %>%
  as.data.frame()


species <- c("SO4","Tot_HNO3","Tot_NH3","OC_secondary","OC_primary","PM", "BC")

tot_conc <- data.frame(matrix(0, ncol = length(species), nrow = nrow(Divisions)))
colnames(tot_conc) <- species

# calibration coefficients -------------------------------------------------

all_pollutants <- c("SO2", "NOX", "NH3", "VOC", "OC", "PM25-PRI", "BC")


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
  read_csv(paste0(emis_dir, all_pollutants[j], '_', height, '.csv'))
}

base_PM <- read_csv(paste0(baseconc_dir, "calibrated_conc.csv")) %>% 
  select(census_id, base_PM25 = PM_25)

all_sectors <-  list.files(emis_main, full.names = F) #FFF sector removed from folder, all emissions are 0

for (k in 1:length(all_sectors)) {
  
  emis_dir <- paste0(emis_main, all_sectors[k], '/')
  
  
  
  for (j in 1:length(all_pollutants)) {
    
    #'Source-receptor matrix for primary organics is the same as primary PM2.5.
    #'It is converted from primary organic aerosol into primary organics at the end
    #'of the script.
    if(all_pollutants[j] %in% c('OC', 'BC')) {
      sr_matrix <- read_matrix('PM25-PRI')
      
    }  
    
    #' Source-receptor matrix for other pollutants
    if (!all_pollutants[j] %in% c('OC', 'BC')) {
      sr_matrix <- read_matrix(all_pollutants[j])
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
  
  uncal_conc <- tot_conc %>% 
    mutate(census_id = Divisions$census_id, # location geography id
           OC_primary = OC_primary/OA_to_OC # convert primary organic aerosol into primary organic carbon since primary PM2.5 matrix was used
    ) %>% # total organics 
    relocate(census_id, .before = SO4) %>% 
    as.data.frame()
  
  
  sector_PM <- local({
  source(paste0(curr_dir, '/scripts/sectors/partition.R'), local = TRUE)
    
  })[["value"]]
 
  
 sector_deaths <- local({
    source(paste0(curr_dir, '/scripts/sectors/sector_mortality.R'), local = TRUE)
    
  })[["value"]]
  
  write.csv(sector_PM, paste0(export_dir1, all_sectors[k], '.csv'))
  write.csv(sector_deaths, paste0(export_dir2, all_sectors[k], '.csv'))
  
  print(k/length(all_sectors) * 100)
  
  rm(sector_PM, sector_deaths)
  
  
}

