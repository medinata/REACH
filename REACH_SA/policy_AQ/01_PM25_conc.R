
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)



# Directories -------------------------------------------------------------
curr_dir <- getwd()
result_file <- paste0(curr_dir,'/results/matrices/SR_' )
baseconc_dir <- paste0(curr_dir, '/results/concentrations/')
script_dir <- paste0(curr_dir, '/scripts/concentrations/')

policy_dir <- paste0(curr_dir, '/policy_AQ/')
emis_main <- paste0(policy_dir, 'new_emis/')
export_dir <- paste0(policy_dir, 'results/PM25_concentration/')
# -------------------------------------------------------------------------

species <- c("SO4","Tot_HNO3","Tot_NH3","OC_secondary","PM")

tot_conc <- data.frame(matrix(0, ncol = length(species), nrow = nrow(Divisions)))
colnames(tot_conc) <- species


all_pollutants <- c("SO2", "NOX", "NH3", "VOC", "PM25-PRI")


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


for (k in 1:length(scenario_names)) {
  
  emis_dir <- paste0(emis_main, scenario_names[k], '/')
  
  pm_dir <- paste0(export_dir, scenario_names[k], '/')
  
  for (j in 1:length(all_pollutants)) {
    
    
    sr_matrix <- read_matrix(all_pollutants[j])
    
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
    mutate(census_id = Divisions$census_id
    ) %>% 
    relocate(census_id, .before = SO4) %>% 
    as.data.frame()
  
  
  policy_PM <- local({
    source(paste0(policy_dir, '/scripts/partition.R'), local = TRUE)
    
  })[["value"]]
  
  
  write.csv(policy_PM, paste0(pm_dir, 'pm_conc.csv'))
 
 
  # reset variables
  rm(policy_PM, emis_dir, pm_dir
     )
  
  
}

