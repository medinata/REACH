# Description -------------------------------------------------------------
#' Input data for marginal social costs
#' Adjust country names, co-exposure factor years, and other inputs listed 
#' below for the domain of interest


# Library -----------------------------------------------------------------
library(tidyverse)


# Inputs ------------------------------------------------------------------
counterfactual <- 2.4 # counterfactual concentration used for GBD concentration-response function
#' baseline deaths from IHD, COPD, lung cancer, LRI and stroke
base_deaths <- read_csv(paste0(mort_dir, '/inputs/mortality_cause_specific.csv'))


# Coexposure factors ------------------------------------------------------
#' GBD concentration-response functions include cohort studies on ambient
#' and household PM2.5. Because REACH estimates ambient PM2.5 concentration,
#' adjust mortality estimates with co-exposure factors to isolate the ambient PM2.5
#' mortality contribution. See reference above for more details
coexposure <- read_csv(paste0(mort_dir, '/inputs/GBD2019_BurdenData/CoExposure_Factors.csv')) %>% 
  rename(country = Location)

#' Adjust for model baseline year and filter out countries in domain
domain_countries <- c("South Africa", "Eswatini", "Angola", "Botswana", "Lesotho",
                      "Malawi", "Mozambique", "Namibia", "Zambia","Zimbabwe")

#' Using the year 2018 for REACH-Southern Africa
#' Assume 2018 coexposure is midpoint of 2017 and 2019 (values are similar and don't introduce significant error)
#' Adjust as needed for the model baseline year
coexposure_adj <- coexposure %>% 
  mutate(adj_factor = (`2017` + `2019`) / 2) %>% 
  filter(country %in% domain_countries) %>% 
  select(country, adj_factor)




