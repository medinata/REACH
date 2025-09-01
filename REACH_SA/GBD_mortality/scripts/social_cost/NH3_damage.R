# Description -------------------------------------------------------------
#' NH3 marginal social costs 


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



domain_info <- read_csv(paste0(input_dir, 'southern_africa.csv'))


# Baseline concentration
PM25_base <- read_csv(paste0(conc_dir, "calibrated_conc.csv")) 

# baseline free ammonium and baseline total nitrate (moles)
molar_base <- read_csv(paste0(conc_dir, "base_molar.csv"))


uncal_conc <- read_csv(paste0(conc_dir, 'uncalibrated_conc.csv')) %>%
  select(census_id, SO4, Tot_HNO3)


# calibration -------------------------------------------------------------
source(paste0(script_dir, "calibration_coefficients.R"))

# GBD relative risk --------------------------------------------------------
source(paste0(rr_dir, 'GBD_relative_risk.R')) #ignore warnings for column names, these correspond to columns that are not used


# pollutant emissions -----------------------------------------------------
poll_var <- 'NH3'
poll_cal <- ifelse(poll_var == 'NOX', NOX_cal, NH3_cal)
open_emis <- function(height) {
  read_csv(paste0(emis_dir, poll_var, '_', height, '.csv'))
}

elevated_emis <- open_emis('elevated')

ground_emis <- open_emis('ground')

poll_emis <- rbind(elevated_emis, ground_emis) 



# source-receptor matrix --------------------------------------------------
#' Sources are rows, receptors are columns (order for each is based on lowest
#' to highest fips code)
sr_matrix <- read_matrix(poll_var)


for (j in 1:nrow(poll_emis)) {
  #' Add 1 ton of emissions to each source at a time
  new_emis <- poll_emis
  new_emis[j,'emis'] <- new_emis[j,'emis'] + 1
  
  tot_emis <- new_emis %>% 
    {as.matrix(.$emis, ncol = 1)}
  
  
  poll_tot <- list()
  
  
  for (i in 1:ncol(sr_matrix)) {
    poll_tot[[i]] <-  sr_matrix[,i] %*% tot_emis
  }
  
  #' Calculate new total nitrate (gas + particle) concentrations (Tot_HNO3) 
  #' based on 1 ton emission to a source
  tot_conc <- data.frame(Tot_NH3 = do.call(rbind, poll_tot)) %>% 
    cbind(uncal_conc) %>% 
    mutate(SO4 = SO4*SO4_cal,
           Tot_HNO3 = Tot_HNO3*NOX_cal,
           Tot_NH3 = Tot_NH3*NH3_cal
    )
  
  #' Add marginal addition ('NO3_add1/NO3_add2') to baseline NO3 ('Base_NO3_mol') 
  #' based on NH4 poor or rich conditions
  #' NO3_add assumes that some additional NO3 is added on top of baseline with no free NH4
  molar_conc <- mutate(tot_conc,
                       Tot_HNO3 = 0.98*Tot_HNO3, #0.98 (HNO3 to NO3)
                       Tot_NH3 = 1.06*Tot_NH3, # 1.06 (NH3 to NH4)
                       Tot_NH3_mol = Tot_NH3/18,
                       SO4_mol = SO4/96,
                       HNO3_mol = Tot_HNO3/62,
                       NH3f_mol = Tot_NH3_mol - (1.5*SO4_mol),
                       NH3f_mol = ifelse(NH3f_mol <= 0, 0, NH3f_mol)) %>% 
    left_join(molar_base, by = 'census_id') %>% 
    mutate(dNH4f = NH3f_mol - Base_NH3f_mol,
           NO3_add1 = (-0.039*dNH4f) + (9.497*Base_TotHNO3_mol*dNH4f),
           NO3_add2 = ( 0.519*dNH4f) + (7.900*Base_TotHNO3_mol*dNH4f),
           NO3_mol = ifelse(Base_NH3f_mol == 0, Base_NO3_mol + NO3_add2, Base_NO3_mol + NO3_add1),
           NO3_mol = ifelse(NO3_mol > HNO3_mol, HNO3_mol, NO3_mol), # Particulate nitrate must not exceed total nitrate (gas + particle)
           NH4_mol = (2*SO4_mol) + NO3_mol, # Assume full neutralization of sulfate and nitrate
           NH4_mol = ifelse(NH4_mol > Tot_NH3_mol, Tot_NH3_mol, NH4_mol) # Particulate ammonium must not exceed total ammonium (gas + particle)
    )
  
  #' Add calibrated baseline concentrations for species not involved in partitioning
  #' and sum up PM2.5 species to get total PM2.5 
  new_conc <- molar_conc %>%
    select(census_id, NH4_mol, NO3_mol) %>% 
    left_join(select(PM25_base, census_id, H2SO4, PM, SOA), by = 'census_id') %>% 
    mutate(NO3 = NO3_mol*62, # umol/m3 to ug/m3
           NH4 = NH4_mol*18, # umol/m3 to ug/m3
           PM_25 = H2SO4 + NO3 + NH4 + SOA + PM #sulfuric acid + nitrate + ammonium + secondary organic aerosol + primary PM2.5
    ) %>% 
    select(census_id, new_PM25 = PM_25)  %>% 
    left_join(domain_info)
  
  #' Get PM2.5 attributed mortalities
  mort_diff <- local({
    source(paste0(msc_dir, 'new_mort.R'), local = TRUE)
    
    
    # All variables created in myscript.R are local to this block
  })[["value"]]
  
  
  poll_emis[j, 'mort_pertonne'] <- mort_diff
  
  if (j %% 100 == 0) {
    print(j/nrow(poll_emis)*100)
  }
  

}  

poll_emis %<>% 
  mutate(mort_pertonne = ifelse(mort_pertonne < 0, 0, mort_pertonne)) # set negative ammonia damages to zero as ammonia damages can't be zero

write_csv(poll_emis, paste0(export_dir,poll_var, '_damage.csv'))

