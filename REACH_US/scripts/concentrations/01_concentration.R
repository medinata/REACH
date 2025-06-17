# Description -------------------------------------------------------------
#' This script applies emissions from the 2017 NEI to the source-receptor matrices to
#' estimate concentrations at all receptor locations 
#' 
#' Estimates concentrations of sulfate, total (gas + particulate) nitrate, total (gas + particulate) ammonia, 
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
proj_dir <- getwd()
input_dir <- paste0(proj_dir,'/files/inputs/')
result_file <- paste0(proj_dir,'/results/matrices/SR_' )
emis_dir <- paste0(proj_dir,'/files/emissions/')
export_dir <- paste0(proj_dir, '/files/concentrations/')

# Inputs ------------------------------------------------------------------
Divisions <- read_csv(paste0(input_dir, "county_2017.csv")) %>%
  as.data.frame()

species <- c("SO4","Tot_HNO3","Tot_NH3","OC_secondary","OC_primary", "PM")

#' Create a dataframe to save concentrations
tot_conc <- data.frame(matrix(0, ncol = length(species), nrow = nrow(Divisions)))
colnames(tot_conc) <- species


poll_info <- c("SO2", "NOX", "NH3", "VOC","OC_primary","PM25-PRI")

#' convert organic carbon to organic aerosol
#' This is applied to primary organic carbon (OC_primary) as the primary PM2.5 matrix
#' was used. Primary PM2.5 is primary organic aerosol (POA), so it has to be converted
#' to OC for primary OC with the OA to OC ratio
OA_to_OC <- 1.8



open_matrix <- function(name_ext, poll) {
  matrix_path <- paste0(result_file,poll,'_')
  fread(paste0(matrix_path,name_ext,'.csv'))
}


#' Open source-receptor matrix. The order of egu, nonegu, and ground follows
#' the same order as the emissions file
read_matrix <- function(poll_name) {
  egu_poll <- open_matrix("egu", poll_name)
  nonegu_poll <- open_matrix("nonegu", poll_name)
  ground_poll <- open_matrix("ground", poll_name)
  
  rbindlist(list(egu_poll,nonegu_poll,ground_poll)) %>% 
    as.matrix()
  
  
}


for (j in 1:length(poll_info)) {
  # SR_matrix (ug/m3/metric ton)
  if(poll_info[j] == 'OC_primary') {
    sr_matrix <- read_matrix('PM25-PRI')
    
  }  
  
  if (poll_info[j] != "OC_primary") {
    sr_matrix <- read_matrix(poll_info[j])
  }
  
  
  # emissions matrix (metric ton)
  tot_emis <- read_csv(paste0(emis_dir,"NEI_2017_",poll_info[j],'_nofires.csv')) %>% 
    {as.matrix(.$emis, ncol = 1)}
  
  poll_tot <- list()
  
  # For every receptor location (column in the matrix), estimate the concentrations (ug/m3)
  for (i in 1:ncol(sr_matrix)) {
    poll_tot[[i]] <-  sr_matrix[,i] %*% tot_emis
  }
  
  # receptor concentrations in ug/m3
  tot_conc[,j] <- do.call(rbind, poll_tot)
  
}

final_conc <- tot_conc %>% 
  mutate(GEOID17 = Divisions$GEOID17,
         OC_primary = OC_primary/OA_to_OC # convert primary organic aerosol into primary organic carbon
        ) %>% 
  relocate(GEOID17, .before = SO4)


write_csv(final_conc, paste0(export_dir, 'uncalibrated_conc.csv'))








