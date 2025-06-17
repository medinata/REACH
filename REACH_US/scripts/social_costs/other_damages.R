# Description -------------------------------------------------------------
#' Estimate primary PM2.5, SO2, and VOC marginal social costs ($/tonne)

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)

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

OA_to_OC <- 1.8 # ratio of organic aerosol to organic carbon

Input <- read_csv(paste0(input_file, "Input.csv")) # Adjust VSL and relative risk (R) 


Divisions <- read_csv(paste0(input_file,"county_2017.csv")) %>%
  select(GEOID17, deaths_30plus = deaths) #' note: rename deaths and pop in other scripts to clarify between total and 30 plus

# Baseline concentrations --------------------------------------------------------
old_conc <- read_csv(paste0(conc_dir, "calibrated_conc.csv")) %>% 
  select(GEOID17, base_PM25 = PM_25) 


poll_info <- data.frame(poll_var = c('PM25-PRI','SO2','VOC'),
                        species = c('PM', 'SO4', 'OC_secondary'))

# calibration -------------------------------------------------------------
source(paste0(script_dir, "calibration_coefficients.R"))



for (j in 1:nrow(poll_info)) {
  poll_emis <- read_csv(paste0(emis_dir, "NEI_2017_", poll_info$poll_var[j], "_nofires.csv"))

  uncal_conc <- read_csv(paste0(conc_dir, "uncalibrated_conc.csv")) %>%
    select(-poll_info$species[j])

  # source-receptor matrix --------------------------------------------------
  #' Sources are rows, receptors are columns (order for each is based on lowest
  #' to highest identifier code)
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

    #' Estimate concentrations at all receptor locations from one tonne
    #' additional emissions at a given source location
    for (i in 1:ncol(sr_matrix)) {
      poll_tot[[i]] <- sr_matrix[, i] %*% tot_emis
    }


    poll_conc <- data.frame(do.call(rbind, poll_tot))
    colnames(poll_conc) <- poll_info$species[j]

    #' Apply calibration constants 
    tot_conc <- cbind(uncal_conc, poll_conc) %>%
      mutate(
        OC_secondary = OC_secondary * OC_cal,
        SO4 = SO4 * SO4_cal,
        Tot_HNO3 = Tot_HNO3 * NOX_cal,
        Tot_NH3 = Tot_NH3 * NH3_cal,
        PM = PM * PM_cal
      )

    #' Inorganic partitioning of total nitrate into nitrate and ammonium estimation
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

    #' Calculate new PM2.5 concentration from 1 tonne change in emissions
    new_conc <- mutate(molar_conc,
      NO3 = NO3_mol * 62, # umol/m3 to ug/m3
      NH4 = NH4_mol * 18,
      SOA = OC_secondary * OA_to_OC,
      H2SO4 = SO4 * 98 / 96,
      PM_25 = H2SO4 + NO3 + NH4 + SOA + PM # sulfuric acid + nitrate + ammonium + secondary organic aerosol + primary PM2.5
    ) %>%
      select(GEOID17, new_PM25 = PM_25)

    #' From the change in PM2.5, calculate marginal social costs
    compare_conc <- left_join(old_conc, new_conc, by = "GEOID17") %>%
      mutate(diff_PM25 = new_PM25 - base_PM25) %>%
      left_join(Divisions, by = "GEOID17") %>%
      mutate(damage = Input$VSL * deaths_30plus * (1 - exp((-log(Input$R) / 10) * diff_PM25)))

    poll_emis[emis_id, "marginal_damage"] <- sum(compare_conc$damage)
    
    #' Code progress tracker
    if (emis_id %% 500 == 0) {
      print(emis_id/nrow(poll_emis)*100)
    }
  }

  #' Export marginal social costs
  write_csv(poll_emis, paste0(export_dir, poll_info$poll_var[j], ".csv"))
}

