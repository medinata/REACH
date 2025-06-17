# Description -------------------------------------------------------------
#' Estimate baseline PM2.5 attributed deaths (summed from these causes: IHD, COPD, lung cancer, LRI and stroke)
#' in each country


# Library -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)

# Directories -------------------------------------------------------------
mort_dir <- paste0(getwd(), '/GBD_mortality/')
script_dir <- paste0(mort_dir, 'scripts/')
conc_dir <- paste0(getwd(),  '/results/concentrations/')
export_dir <- paste0(mort_dir, 'results/')


# inputs ------------------------------------------------------------------
counterfactual <- 2.4 


#' baseline deaths from IHD, COPD, lung cancer, LRI and stroke
base_deaths <- read_csv(paste0(mort_dir, '/inputs/mortality_cause_specific.csv'))


#' Baseline PM2.5 concentration 
REACH_PM25 <- read_csv(paste0(conc_dir, 'calibrated_conc.csv')) %>% 
  select(census_id, geo_name, base_PM25 = PM_25)

#' GBD relative risk for each cause
source(paste0(script_dir, 'GBD_relative_risk.R'))

# PM2.5 deaths (IHD, DM, COPD, LC) ----------------------------------------
codes <- data.frame(cause = c("COPD", "DM", "LRI", "LC"),
                    cause_name = c("Chronic obstructive pulmonary disease",
                                   "Diabetes mellitus",
                                   "Lower respiratory infections",
                                   "Tracheal, bronchus, and lung cancer"
                    )
)
#' Interpolate to get REACH RR from GBD
interpolate_list <- list()

for (j in 1:length(diseases_1)) {
  cause_RR <- RR_curve %>% 
    select(PM_conc, RR = diseases_1[j]) 
  
  interpolate_list[[j]] <- REACH_PM25 %>% 
    mutate(RR =  spline(x = cause_RR$PM_conc, y = cause_RR$RR, xout = base_PM25)$y,
           cause = diseases_1[j]
    )
}

other_PAF <- do.call(rbind, interpolate_list)  %>% 
  mutate(RR = ifelse(base_PM25 <= counterfactual, 1, RR), # for concentrations less than 2.4, the RR is exactly 1, so don't use interpolated value
         PAF = 1-(1/RR)) %>% 
  left_join(codes)

#' baseline deaths from each cause
other_basedeaths <- base_deaths %>%
  group_by(geo_name, cause_name, country) %>%
  summarise(tot_mort = sum(deaths)) %>%
  filter(cause_name %in% codes$cause_name)

#' baseline PM2.5 attributed deaths
other_deaths <- left_join(other_basedeaths, other_PAF, by = c('geo_name', 'cause_name')) %>% 
  mutate(PM_excess = tot_mort*PAF) %>% 
 group_by(country, cause_name) %>% 
 summarise(PM_mort = sum(PM_excess))


# PM2.5 deaths (stroke, IHD) ----------------------------------------------
#' Interpolate RR for age-specific causes 
age_groups <- colnames(stroke_RR)[1:15]

other_RR <- rbind(stroke_RR, IHD_RR)
diseases_2 <- c('stroke', 'IHD')


obtain_RR <- function(cause_name) {
  RR_list <- list()
  
  for (age_ind in 1:length(age_groups)) {
    RR_info <- other_RR %>% 
      filter(disease == cause_name) %>% 
      select(PM_conc, age_groups[age_ind]) %>% 
      rename(RR = 2)
    
    RR_list[[age_ind]] <- REACH_PM25 %>% 
      mutate(RR =  spline(x = RR_info$PM_conc, y = RR_info$RR, xout = base_PM25)$y,
             cause = cause_name,
             age_group = age_groups[age_ind]
      )
    
  }
  
  do.call(rbind, RR_list)
}

# Interpolated RR for each location in domain
REACH_strokeRR <- obtain_RR('stroke')
REACH_IHDRR <- obtain_RR('IHD')


get_others <- function(code, df_name) {
  disease_RR <- df_name %>% 
    mutate(RR = ifelse(base_PM25 <= counterfactual, 1, RR), # for concentrations less than 2.4, the RR is exactly 1, so don't use interpolated value
           PAF = 1-(1/RR))   %>% # population attributed fraction
    mutate(age_group = ifelse(age_group == '80-84 years', '80+ years', age_group)) %>% # format the same as mortality file
    filter(!age_group %in% c("85-89 years", "90-94 years", "95+ years"))
  
  # For age groups 75 +, assume RR for ages 75-79 because population data is not
  # available for age groups "80-84 years" "85-89 years" "90-94 years" "95+ years".
  # If population data for these groups are available for the domain, remove this
  # part of the code
  RR_75plus <- disease_RR %>% 
    filter(age_group == '75-79 years') %>% 
    mutate(age_group = '75+ years')
  
  disease_RR %<>%
    rbind(RR_75plus) %>% 
    select(geo_name, age_group, PAF)

  disease_base <- filter(base_deaths,
                         cause_name == code) %>%
    left_join(disease_RR, by = c('geo_name', 'age_group')) %>%
    mutate(PM_excess = deaths*PAF)

  disease_base %>%
    group_by(country, cause_name) %>%
    summarise(PM_mort = sum(PM_excess))
}

stroke_deaths <- get_others('Stroke', REACH_strokeRR)
IHD_deaths <- get_others( 'Ischemic heart disease', REACH_IHDRR)

all_deaths <- rbind(other_deaths, stroke_deaths, IHD_deaths)

write_csv(all_deaths, paste0(export_dir, 'basePM_mort.csv'))

# Compare PM2.5 mortalities -----------------------------------------------
country_deaths <- all_deaths %>% 
  group_by(country) %>% 
    summarise(REACH_mort = sum(PM_mort))

countries <- country_deaths$country
mcduffie <- read_excel(paste0(mort_dir, '/mcduffie_2021/mortalities.xlsx'), skip = 6) %>% 
  select(country = 1, PM25 = 2, GBD_deaths = 23, GEMM_deaths = 26) %>% 
  filter(country %in% countries) %>% 
  mutate(PM_avg = as.numeric(PM25),
         GBD_deaths = as.numeric(GBD_deaths))

compare_deaths <- read_csv(paste0(mort_dir, '/inputs/GBD2019_BurdenData/CoExposure_Factors.csv')) %>% 
  rename(country = 1) %>% 
  mutate(country = ifelse(country == 'United Republic of Tanzania', 'Tanzania', country)) %>% 
  filter(country %in% countries) %>% 
  left_join(mcduffie) %>% 
  left_join(country_deaths) %>% 
  mutate(REACH_adjmort = REACH_mort * `2017`)
