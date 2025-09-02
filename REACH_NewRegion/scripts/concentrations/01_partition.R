# Description -------------------------------------------------------------
#' This script partitions total ammonia and total nitrate into particulate ammonium 
#' and particulate nitrate 
#' Script also estimates baseline PM2.5 concentrations at all receptor locations
#' (aggregate of primary PM2.5, sulfate, nitrate, ammonium and secondary organic aerosols)
#' Calibration coefficients are applied to limit mean bias between model predictions
#' and observations (see REACH paper for more details)


# Variables ---------------------------------------------------------------
#' All units are ug/m3 unless variable contains "_mol" where the units are umol/m3 or variable
#' is a calibration coefficient ('_cal')
#' 
#' OC_tot: primary plus secondary organic carbon
#' OC_primary: primary organic carbon
#' OC_secondary: secondary organic carbon
#' SO4: sulfate
#' Tot_HNO3: total nitrate (particulate NO3- plus gas HNO3)
#' Tot_NH3: total ammonia (particulate NH4+ plus gas NH3)
#' PM: primary PM2.5
#' NH3f: free ammonia - ammonia available after neutralizing sulfate
#' NO3: particulate nitrate
#' NH4: particulate ammonium 
#' SOA: secondary organic aerosols
#' H2SO4: sulfuric acid (preferred form of sulfate)
#' PM_25: baseline total PM2.5
#' OA_to_OC: organic aerosol to organic carbon ratio

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Directories -------------------------------------------------------------
proj_dir <- getwd()
input_dir <- paste0(proj_dir,'/inputs/')
result_dir <- paste0(proj_dir, '/results/concentrations/')
script_dir <- paste0(proj_dir, '/scripts/concentrations/')


# Inputs ------------------------------------------------------------------
Divisions <- read_csv(paste0(input_dir,"southern_africa.csv")) %>%
  as.data.frame()


OA_to_OC <- 1.8

#' There is limited speciated measurements in Southern Africa, so REACH-US 
#' calibration coefficients were assumed for the region
source(paste0(script_dir, 'calibration_coefficients.R'))


# Apply calibration coefficients to model predictions
tot_conc <- read_csv( paste0(result_dir, 'uncalibrated_conc.csv')) %>% 
  mutate(OC_primary = OC_primary*PM_cal,
         OC_secondary = OC_secondary*OC_cal,
         OC_tot = OC_primary + OC_secondary,
         SO4 = SO4*SO4_cal,
         Tot_HNO3 = Tot_HNO3*NOX_cal,
         Tot_NH3 = Tot_NH3*NH3_cal,
         PM = PM*PM_cal,
         BC = BC*PM_cal
  )

# partition ---------------------------------------------------------------
#' Estimate particulate nitrate from sulfate and free ammonia by applying
#' regression from the APEEP model. The particulate nitrate regression was
#' derived from fits to CAMx

molar_conc <- mutate(tot_conc,
                     Tot_HNO3 = 0.98*Tot_HNO3, #0.98 (HNO3 to NO3)
                     Tot_NH3 = 1.06*Tot_NH3, # 1.06 (NH3 to NH4)
                     Tot_NH3_mol = Tot_NH3/18, #total ammonia ug/m3 to umol/m3
                     SO4_mol = SO4/96, #sulfate ug/m3 to umol/m3
                     Tot_HNO3_mol = Tot_HNO3/62, #total nitrate ug/m3 to umol/m3
                     NH3f_mol = Tot_NH3_mol - (1.5*SO4_mol), # free ammonia, ug/m3 to umol/m3
                     NH3f_mol = ifelse(NH3f_mol <= 0, 1E-20, NH3f_mol),
                     #' particulate nitrate regression from APEEP (derived from CAMx fits):
                     NO3_mol = 0.6509*((0.33873*Tot_HNO3_mol) + (0.121008*NH3f_mol) + (3.511482*NH3f_mol*Tot_HNO3_mol)), 
                     NO3_mol = ifelse(NO3_mol > Tot_HNO3_mol, Tot_HNO3_mol, NO3_mol), # Constraint to not allow particulate nitrate to exceed the total nitrate
                     NH4_mol = (2*SO4_mol) + NO3_mol, # Assume full neutralization of sulfate and nitrate
                     NH4_mol = ifelse(NH4_mol > Tot_NH3_mol, Tot_NH3_mol, NH4_mol) # Constraint to not allow particulate ammonium to exceed the total ammonia
)

#' Convert from molar units to mass units for species involved in partitioning and 
#' aggregate PM2.5 species concentrations for each receptor location
final_conc <- mutate(molar_conc, 
                     NO3 = NO3_mol*62, # umol/m3 to ug/m3
                     NH4 = NH4_mol*18, # umol/m3 to ug/m3
                     SOA = OC_secondary*OA_to_OC, 
                     H2SO4 = SO4*98/96,
                     PM_25 = H2SO4 + NO3 + NH4 + SOA + PM #Aggregate total PM2.5: sulfuric acid + nitrate + ammonium + secondary organic aerosol + primary PM2.5
) %>% 
  select(census_id, H2SO4, NO3, NH4, SOA, PM, PM_25, OC_tot, OC_primary, OC_secondary) %>% 
  left_join(Divisions[,c('census_id','geo_name','country')])



# Export results ----------------------------------------------------------
write_csv(final_conc, paste0(result_dir, "calibrated_conc.csv"))


#' Save molar concentrations for use in estimating the NH3 and NOx marginal social costs
molar_conc %>% 
  select(census_id, Base_TotHNO3_mol = Tot_HNO3_mol, Base_NH3f_mol = NH3f_mol,
         Base_NO3_mol = NO3_mol, Base_SO4_mol = SO4_mol,
         Base_TotNH3_mol = Tot_NH3_mol) %>% 
write_csv(paste0(result_dir, "base_molar.csv"))


