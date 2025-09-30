

OA_to_OC <- 1.8

# REACH-US calibration coefficeints
source(paste0(script_dir, 'calibration_coefficients.R'))

poll_info <- data.frame(
  poll = c("SO2", "NOX", "NH3", "VOC","PM25-PRI"),
  cal = c(SO4_cal, NOX_cal, NH3_cal, OC_cal, PM_cal)
)


# Apply calibrarion coefficients to model predictions
calibrated_conc <- uncal_conc %>% 
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

molar_conc <- mutate(calibrated_conc,
                     Tot_HNO3 = 0.98*Tot_HNO3, #0.98 (HNO3 to NO3)
                     Tot_NH3 = 1.06*Tot_NH3, # 1.06 (NH3 to NH4)
                     Tot_NH3_mol = Tot_NH3/18, #total ammonia ug/m3 to umol/m3
                     SO4_mol = SO4/96, #sulfate ug/m3 to umol/m3
                     HNO3_mol = Tot_HNO3/62, #total nitrate ug/m3 to umol/m3
                     NH3f_mol = Tot_NH3_mol - (1.5*SO4_mol), # free ammonia, ug/m3 to umol/m3
                     NH3f_mol = ifelse(NH3f_mol <= 0, 1E-20, NH3f_mol),
                     #' particulate nitrate regression from APEEP (derived from CAMx fits):
                     NO3_mol = 0.6509*((0.33873*HNO3_mol) + (0.121008*NH3f_mol) + (3.511482*NH3f_mol*HNO3_mol)), 
                     NO3_mol = ifelse(NO3_mol > HNO3_mol, HNO3_mol, NO3_mol), # Constraint to not allow particulate nitrate to exceed the total nitrate
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
  select(census_id, new_PM25 = PM_25) %>% 
  left_join(Divisions[,c('census_id','geo_name','country')])



left_join(final_conc, base_PM) %>% 
  mutate(PM_sector = base_PM25 - new_PM25,
         sector = all_sectors[k])
