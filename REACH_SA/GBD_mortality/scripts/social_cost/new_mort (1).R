# Description -------------------------------------------------------------
#' Estimate marginal social costs (difference in mortalities from baseline after
#' adding a tonne of PM2.5 precursor to a source location)


# Library -----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)
library(stringr)

# Directories -------------------------------------------------------------
mort_dir <- paste0(getwd(), '/GBD_mortality/')
basemort_dir <- paste0(mort_dir, 'results/')
# inputs ------------------------------------------------------------------
counterfactual <- 2.4 


#' baseline deaths from all causes 
base_deaths <- read_csv(paste0(mort_dir, '/inputs/mortality_cause_specific.csv'))


#' New PM2.5 concentration (from one 1 tonne addition to one source location)
REACH_PM25 <- new_conc %>% 
  select(census_id, geo_name, new_PM25)


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
    mutate(RR =  spline(x = cause_RR$PM_conc, y = cause_RR$RR, xout = new_PM25)$y,
           cause = diseases_1[j]
    )
}

other_PAF <- do.call(rbind, interpolate_list)  %>% 
  mutate(RR = ifelse(new_PM25 <= counterfactual, 1, RR), # for concentrations less than 2.4, the RR is exactly 1, so don't use interpolated value
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
      mutate(RR =  spline(x = RR_info$PM_conc, y = RR_info$RR, xout = new_PM25)$y,
             cause = cause_name,
             age_group = age_groups[age_ind]
      )
    
  }
  
  do.call(rbind, RR_list)
}

REACH_strokeRR <- obtain_RR('stroke')
REACH_IHDRR <- obtain_RR('IHD')


get_others <- function(code, df_name) {
  disease_RR <- df_name %>% 
    mutate(RR = ifelse(new_PM25 <= counterfactual, 1, RR), # for concentrations less than 2.4, the RR is exactly 1, so don't use interpolated value
           PAF = 1-(1/RR))    %>% # population attributed fraction
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

all_deaths <- rbind(other_deaths, stroke_deaths, IHD_deaths) %>% 
  rename(new_mort = PM_mort)


base_deaths <- read_csv(paste0(basemort_dir, 'basePM_mort.csv')) %>% 
  rename(base_mort = PM_mort) 

compare_deaths <- left_join(all_deaths, base_deaths, by = c("country", "cause_name")) %>% 
  mutate(diff = new_mort - base_mort)

sum(compare_deaths$diff)
