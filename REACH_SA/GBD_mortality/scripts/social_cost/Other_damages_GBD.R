# VOC, Sulfate, primary PM2.5 damages

# Description -------------------------------------------------------------
#' 02/14/25: Used updated 2018 age-specific mortality rates by country from the GBD
#' 2021. Previous baseline mortality rates were not age-specific and were for 2017 
#' 
#' Estimate health damages of PM2.5, VOCs, and SO4. Use this method for SO4
#' because partitioning affects the marginal and the same method for PM2.5 and VOC
#' for coding consistencies 

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(magrittr)

# Directories -------------------------------------------------------------
result_file <- paste0(getwd(),'/results/matrices/SR_' )
input_dir <- paste0(getwd(),'/inputs/')
emis_dir <- paste0(input_dir,'emissions/processed_emissions/')
conc_dir <- paste0(getwd(), '/results/concentrations/')
script_dir <- paste0(getwd(), '/scripts/concentrations/')
msc_dir <- paste0(getwd(), '/GBD_mortality/scripts/social_cost/')
rr_dir <- paste0(getwd(), '/GBD_mortality/scripts/')
export_dir <- paste0(getwd(), '/results/social_cost/')


# Inputs ------------------------------------------------------------------

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

save_damage <- function(height_var) {
  
  filter(cal_damage, type == height_var) %>% 
    select(census_id, mort_perton,emis, poll, damage_within, height = type) %>% 
    write_csv(paste0(export_dir, poll_var,'_',height_var,'_mort_perton.csv'))
  
}

open_emis <- function(height) {
  read_csv(paste0(emis_dir, poll_var, '_', height, '.csv'))
}



domain_info <- read_csv(paste0(input_dir, 'southern_africa.csv'))




# GBD relative risk --------------------------------------------------------
source(paste0(rr_dir, 'GBD_relative_risk.R')) #ignore warnings for column names, these correspond to columns that are not used

# calibration -------------------------------------------------------------
source(paste0(script_dir, "calibration_coefficients.R"))

OA_to_OC <- 1.8


for (j in 1:nrow(poll_info)) {
  poll_var <- poll_info$poll_var[j]
  
  elevated_emis <- open_emis('elevated')
  
  ground_emis <- open_emis('ground')
  
  poll_emis <- rbind(elevated_emis, ground_emis) %>%
               mutate(mort_pertonne = NA_real_)
  
  
  uncal_conc <- read_csv(paste0(conc_dir, "uncalibrated_conc.csv")) %>%
    select(-poll_info$species[j])
  
  # source-receptor matrix --------------------------------------------------
  #' Sources are rows, receptors are columns (order for each is based on lowest
  #' to highest fips code)
  #' 
  sr_matrix <- read_matrix(poll_info$poll_var[j])

  
  
  for (emis_id in 1:nrow(poll_emis)) {
    #' Add 1 ton of emissions to each source at a time
    
    new_emis <- poll_emis
    new_emis[emis_id, "emis"] <- new_emis[emis_id, "emis"] + 1
    
    tot_emis <- new_emis %>% {
      as.matrix(.$emis, ncol = 1)
    }
    
    
    poll_tot <- list()
    
    
    for (i in 1:ncol(sr_matrix)) {
      poll_tot[[i]] <- sr_matrix[, i] %*% tot_emis
    }
    
    
    poll_conc <- data.frame(do.call(rbind, poll_tot))
    colnames(poll_conc) <- poll_info$species[j]
    
    # if (poll_info$species[j] == 'OC_primary') {
    #   poll_conc %<>%
    #     mutate(OC_primary = OC_primary/OA_to_OC) #Used PM2.5 S-R matrix, so convert to OC units
    #   
    # }
    
    
    tot_conc <- cbind(uncal_conc, poll_conc) %>%
      mutate(
        OC_secondary = OC_secondary * OC_cal,
        SO4 = SO4 * SO4_cal,
        Tot_HNO3 = Tot_HNO3 * NOX_cal,
        Tot_NH3 = Tot_NH3 * NH3_cal,
        PM = PM * PM_cal
      )
    
    
    molar_conc <- mutate(tot_conc,
                         Tot_HNO3 = 0.98 * Tot_HNO3, # 0.98 (HNO3 to NO3)
                         Tot_NH3 = 1.06 * Tot_NH3, # 1.06 (NH3 to NH4)
                         Tot_NH3_mol = Tot_NH3 / 18,
                         SO4_mol = SO4 / 96,
                         HNO3_mol = Tot_HNO3 / 62, # ug/m3 to umol/m3
                         NH3f_mol = Tot_NH3_mol - (1.5 * SO4_mol), # free ammonia, ug/m3 to umol/m3
                         NH3f_mol = ifelse(NH3f_mol <= 0, 0, NH3f_mol),
                         NO3_mol = 0.6509 * ((0.33873 * HNO3_mol) + (0.121008 * NH3f_mol) + (3.511482 * NH3f_mol * HNO3_mol)), # nitrate regression
                         NO3_mol = ifelse(NO3_mol > HNO3_mol, HNO3_mol, NO3_mol),
                         NH4_mol = (2 * SO4_mol) + NO3_mol, # Assume full neutralization of sulfate and nitrate
                         NH4_mol = ifelse(NH4_mol > Tot_NH3_mol, Tot_NH3_mol, NH4_mol)
    )
    
    
    new_conc <- mutate(molar_conc,
                       NO3 = NO3_mol * 62, # umol/m3 to ug/m3
                       NH4 = NH4_mol * 18,
                       SOA = OC_secondary * OA_to_OC,
                       H2SO4 = SO4 * 98 / 96,
                       PM_25 = H2SO4 + NO3 + NH4 + SOA + PM # sulfuric acid + nitrate + ammonium + secondary organic aerosol + primary PM2.5
    ) %>%
      select(census_id, new_PM25 = PM_25)  %>% 
      left_join(domain_info)
    
    #' Get PM2.5 attributed mortalities
     mort_diff <- local({
      source(paste0(msc_dir, 'new_mort.R'), local = TRUE)
      
    
      # All variables created in myscript.R are local to this block
    })[["value"]]
    
    
     poll_emis[emis_id, 'mort_pertonne'] <- mort_diff
    
    if (emis_id %% 500 == 0) {
      print(emis_id/nrow(poll_emis)*100)
    }
  
    
    
  }
  
  write_csv(poll_emis, paste0(export_dir,poll_var, '_damage.csv'))
  
}


