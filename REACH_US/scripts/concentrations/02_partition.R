# Description -------------------------------------------------------------
#' This script partitions total ammonia and total nitrate into particulate ammonium 
#' and particulate nitrate 
#' The particulate nitrate regression is from AP3
#' Script also estimates baseline PM2.5 concentrations at all receptor locations
#' (aggregate of primary PM2.5, sulfate, nitrate, ammonium and secondary organic aerosols)
#' Calibration coefficients are applied to limit mean bias between model predictions
#' and observations (see REACH paper for more details)


# Variables ---------------------------------------------------------------
#' All units are ug/m3 unless variable contains "_mol" where the units are umol/m3 or variable
#' is a calibration coefficient/constant ('_cal')
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
library(viridis)
library(ggpubr)
library(magrittr)

# Directories --------------------------------------------------------------
proj_dir <- getwd()
input_dir <- paste0(proj_dir,'/inputs/')
plot_dir <- paste0(proj_dir,'/plots/evaluation/')
script_dir <- paste0(proj_dir, '/scripts/')
conc_dir <- paste0(proj_dir, '/results/concentrations/')

# Inputs ------------------------------------------------------------------
source(paste0(script_dir,'03_evaluation.R')) 
source(paste0(script_dir, "calibration_coefficients.R"))

observations <- read_csv(paste0(input_dir, 'obs_info.csv')) %>% 
  mutate(species = ifelse(species == 'OC', 'OC_tot', species))

Divisions <- read_csv(paste0(input_dir,"county_2017.csv")) %>%
  rename(Longitude = lon) %>% 
  as.data.frame()


OA_to_OC <- 1.8


poll_info <- data.frame(
  poll = c("SO2", "NOX", "NH3", "VOC","PM25-PRI"),
  cal = c(SO4_cal, NOX_cal, NH3_cal, OC_cal, PM_cal)
)


#' Apply calibration coefficients to model predictions
tot_conc <- read_csv( paste0(conc_dir, 'uncalibrated_conc.csv')) %>% 
  mutate(OC_primary = OC_primary*PM_cal,
         OC_secondary = OC_secondary*OC_cal,
         OC_tot = OC_primary + OC_secondary,
         SO4 = SO4*SO4_cal,
         Tot_HNO3 = Tot_HNO3*NOX_cal,
         Tot_NH3 = Tot_NH3*NH3_cal,
         PM = PM*PM_cal
  )

# partition ---------------------------------------------------------------
#' Estimate particulate nitrate from total nitrate and free ammonia by applying
#' regression from the AP3 model. The particulate nitrate regression was
#' derived from fits to CAMx
molar_conc <- mutate(tot_conc,
                     Tot_HNO3 = 0.98*Tot_HNO3, #0.98 (HNO3 to NO3)
                     Tot_NH3 = 1.06*Tot_NH3, # 1.06 (NH3 to NH4)
                     Tot_NH3_mol = Tot_NH3/18,  #ug/m3 to umol/m3
                     SO4_mol = SO4/96, #ug/m3 to umol/m3
                     HNO3_mol = Tot_HNO3/62, #ug/m3 to umol/m3
                     NH3f_mol = Tot_NH3_mol - (1.5*SO4_mol), # free ammonia, ug/m3 to umol/m3
                     NH3f_mol = ifelse(NH3f_mol <= 0, 0, NH3f_mol),
                     NO3_mol = 0.6509*((0.33873*HNO3_mol) + (0.121008*NH3f_mol) + (3.511482*NH3f_mol*HNO3_mol)), # particulate nitrate regression from AP3
                     NO3_mol = ifelse(NO3_mol > HNO3_mol, HNO3_mol, NO3_mol), 
                     NH4_mol = (2*SO4_mol) + NO3_mol, # Assume full neutralization of sulfate and nitrate
                     NH4_mol = ifelse(NH4_mol > Tot_NH3_mol, Tot_NH3_mol, NH4_mol) 
) %>% 
  left_join(Divisions[, c("GEOID17", "Longitude")])


#' Estimate baseline total PM2.5 concentrations
final_conc <- mutate(molar_conc, 
                     NO3 = NO3_mol*62, # umol/m3 to ug/m3
                     NH4 = NH4_mol*18, # umol/m3 to ug/m3
                     SOA = OC_secondary*OA_to_OC, #secondary organic carbon to secondary organic aerosol
                     H2SO4 = SO4*98/96, 
                     PM_25 = H2SO4 + NO3 + NH4 + SOA + PM #sulfuric acid + nitrate + ammonium + secondary organic aerosol + primary PM2.5
) %>% 
  select(GEOID17, Longitude, H2SO4, NO3, NH4, OC_tot, SOA, SO4,PM, PM_25)


# evaluation --------------------------------------------------------------
#' plot model predictions against observations
list('SO4','NO3','OC_tot','NH4','PM_25') %>%
  map(evaluate_model)


#' Save molar concentrations for use in estimating the NH3 and NOx marginal social costs
write_csv(final_conc, paste0(conc_dir, "calibrated_conc.csv"))

molar_conc %>%
  select(GEOID17, Base_HNO3_mol = HNO3_mol, Base_NH3f_mol = NH3f_mol,
         Base_NO3_mol = NO3_mol) %>%
  write_csv(paste0(conc_dir, "baseline_molar.csv"))




# stats -------------------------------------------------------------------
# M - mean, E - error, B- bias, F-fractional
summary_stats <- data.frame(species_name = c('SO4','NH4','NO3','OC_tot','PM_25'),
                            obs_mean = NA,
                            model_mean = NA,
                            ME = NA,
                            MB = NA,
                            MFE = NA,
                            MFB = NA
)




for(j in 1:nrow(summary_stats)) {
  compare_conc <- filter(observations, species == summary_stats$species_name[j]) %>% 
    left_join(final_conc[,c('GEOID17',summary_stats$species_name[j],'Longitude')]) %>% 
    rename(model = 5) %>% 
    filter(!is.na(Longitude)) %>% 
    mutate(x_plus = model + observations,
           x_minus = model - observations)
  
  summary_stats$obs_mean[j] <- mean(compare_conc$observations)                                                              
  summary_stats$model_mean[j] <- mean(compare_conc$model)
  
  summary_stats$ME[j] <- sum(abs(compare_conc$x_minus))/nrow(compare_conc)
  summary_stats$MB[j] <- sum(compare_conc$x_minus)/nrow(compare_conc)
  summary_stats$MFE[j] <- sum(2*(abs(compare_conc$x_minus))/(compare_conc$x_plus))/nrow(compare_conc)
  summary_stats$MFB[j] <- sum(2*compare_conc$x_minus/compare_conc$x_plus)/nrow(compare_conc)
  
}


summary_stats[,c(2:7)] <- round(summary_stats[,c(2:7)], digits = 2)
View(summary_stats)


write_csv(summary_stats, paste0(conc_dir, 'eval_stats.csv'))











