# Description -------------------------------------------------------------
#' Script for NH3 marginal social costs ($/tonne)
#' Equations for marginal nitrate addition are from AP4;
#' Equation for baseline nitrate is from AP3

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(magrittr)

# Directories -------------------------------------------------------------
proj_dir <- getwd()
input_file <- paste0(proj_dir,'/inputs/')
result_file <- paste0(proj_dir,'/results/matrices/SR_' )
emis_dir <- paste0(proj_dir,'/inputs/emissions/')
conc_dir <- paste0(proj_dir, '/results/concentrations/')
script_dir <- paste0(proj_dir, '/scripts/concentrations/')
export_dir <- paste0(proj_dir, '/results/social_costs/')


# Inputs ------------------------------------------------------------------
open_matrix <- function(name_ext, poll) {
  matrix_path <- paste0(result_file,poll,'_')
  fread(paste0(matrix_path,name_ext,'.csv'))
}

read_matrix <- function(poll_name) {
  egu_poll <- open_matrix("egu", poll_name)
  nonegu_poll <- open_matrix("nonegu", poll_name)
  ground_poll <- open_matrix("ground", poll_name)
  
  rbindlist(list(egu_poll,nonegu_poll,ground_poll)) %>% 
    as.matrix()
}

Input <- read_csv(paste0(input_file, "Input.csv"))


Divisions <- read_csv(paste0(input_file,"county_2017.csv")) %>%
  select(GEOID17, deaths_30plus = deaths) #' note: rename deaths and pop in other scripts to clarify between total and 30 plus

# Baseline concentrations --------------------------------------------------------
# baseline mass concentrations
PM25_base <- read_csv(paste0(conc_dir, "calibrated_conc.csv"))  


old_conc <- PM25_base %>% 
  select(GEOID17, base_PM25 = PM_25) 


# # baseline molar concentrations
molar_base <- read_csv(paste0(conc_dir, "baseline_molar.csv"))



uncal_conc <- read_csv(paste0(conc_dir, 'uncalibrated_conc.csv')) %>% 
  select(GEOID17, SO4, Tot_HNO3)

# calibration -------------------------------------------------------------
source(paste0(script_dir, "calibration_coefficients.R"))


# pollutant info ----------------------------------------------------------
poll_var <- 'NH3'

# pollutant emissions -----------------------------------------------------
poll_emis <- read_csv(paste0(emis_dir,"NEI_2017_", poll_var,"_nofires.csv"))


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
    left_join(molar_base, by = 'GEOID17') %>% 
    mutate(dNH4f = NH3f_mol - Base_NH3f_mol,
           NO3_add1 = (-0.039*dNH4f) + (9.497*Base_HNO3_mol*dNH4f),
           NO3_add2 = ( 0.519*dNH4f) + (7.900*Base_HNO3_mol*dNH4f),
           NO3_mol = ifelse(Base_NH3f_mol == 0, Base_NO3_mol + NO3_add2, Base_NO3_mol + NO3_add1),
           NO3_mol = ifelse(NO3_mol > HNO3_mol, HNO3_mol, NO3_mol), # Particulate nitrate must not exceed total nitrate (gas + particle)
           NH4_mol = (2*SO4_mol) + NO3_mol, # Assume full neutralization of sulfate and nitrate
           NH4_mol = ifelse(NH4_mol > Tot_NH3_mol, Tot_NH3_mol, NH4_mol) # Particulate ammonium must not exceed total ammonium (gas + particle)
    )
  
  #' Add calibrated baseline concentrations for species not involved in partitioning
  #' and sum up PM2.5 species to get total PM2.5 
  new_conc <- molar_conc %>%
    select(GEOID17, NH4_mol, NO3_mol) %>% 
    left_join(select(PM25_base, GEOID17, H2SO4, PM, SOA), by = 'GEOID17') %>% 
    mutate(NO3 = NO3_mol*62, # umol/m3 to ug/m3
           NH4 = NH4_mol*18,
           PM_25 = H2SO4 + NO3 + NH4 + SOA + PM #sulfuric acid + nitrate + ammonium + secondary organic aerosol + primary PM2.5
    ) %>%
    select(GEOID17, new_PM25 = PM_25)
  
  compare_conc <- left_join(old_conc, new_conc, by = "GEOID17") %>%
    mutate(diff_PM25 = new_PM25 - base_PM25) %>%
    left_join(Divisions, by = "GEOID17") %>%
    mutate(damage = Input$VSL * deaths_30plus * (1 - exp((-log(Input$R) / 10) * diff_PM25)))
  
  
  poll_emis[j, "marginal_damage"] <- sum(compare_conc$damage)
  
  # Set negative ammonia damages to 0 (see REACH paper for details)
  poll_emis %<>%
    mutate(marginal_damage = ifelse(marginal_damage < 0, 0, marginal_damage))
  
  if (j %% 500 == 0) {
    print(j/nrow(poll_emis)*100)
  }
  
  
}  


write_csv(poll_emis, paste0(export_dir, poll_var, '.csv'))



  