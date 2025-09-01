# Description -------------------------------------------------------------
#' Calculate baseline deaths from these specific causes: IHD, COPD, lung cancer, LRI and stroke
#' For all causes except LRI (ages 25 +)
#' LRI (under age 5 and above age 25)

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(sf)
library(ggthemes)

# Directories -------------------------------------------------------------
open_dir <- function(name_ext) {
  paste0(getwd(),'/inputs/', name_ext, '/')
}
pop_dir <- open_dir('population')
sf_dir <- open_dir('shapefiles')
mort_dir <- paste0(getwd(), '/GBD_mortality/inputs/')
input_dir <- paste0(getwd(), '/inputs/')
# Cause-specific mortality rate -------------------------------------------
mortality_LRI <- read_csv(paste0(mort_dir, 'IHME-GBD_2021_DATA_LRI_under5.csv'))
other_mortality <- read_csv(paste0(mort_dir, 'IHME-GBD_2021_DATA_mortality_rates.csv'))

mortality <- rbind(mortality_LRI, other_mortality) %>% 
  select(country = location_name,age_group = age_name,cause_name,year,val) %>%
  mutate(country = str_remove(country, "^(Republic|Kingdom) of "),
         country = toupper(country),
         BMR_person = val/100E3)  %>% # mortality rate per person
  select(-val) 


# Population --------------------------------------------------------------
pop_info <- read_csv(paste0(pop_dir,'popbyage.csv'))


# Mortality ---------------------------------------------------------------
base_mortality <- left_join(pop_info, mortality, by = c('country', 'age_group')) %>% 
  mutate(deaths = pop*BMR_person) %>% 
  select(-country)


sa_info <- read_csv(paste0(input_dir, 'southern_africa.csv')) 


# Export file -------------------------------------------------------------
#' These locations are lakes and have no population info from the Us Census Bureau,
#' so deaths of 0 are assigned
loc_info <- c('MALAWI_LAKE MALAWI_LAKE MALAWI', 'MOZAMBIQUE_LAKE MALAWI_LAKE MALAWI_LAKE MALAWI')
missing_location <- expand.grid(geo_name = loc_info, age_group = unique(base_mortality$age_group),
                                                                        cause_name = unique(base_mortality$cause_name)) %>% 
  mutate(pop = 0,
         BMR_person = 0,
         deaths = 0,
         year = 2018) %>% 
  filter(age_group != 'B75PL_2018') %>% 
  filter(!(age_group == '<5 years' & cause_name != 'Lower respiratory infections'))

mort_info <- rbind(base_mortality, missing_location) %>% 
  left_join(sa_info)


write_csv(mort_info, paste0(mort_dir, 'mortality_cause_specific.csv'))


