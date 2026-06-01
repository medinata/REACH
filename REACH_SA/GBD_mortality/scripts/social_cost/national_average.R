# Description -------------------------------------------------------------
#' Emission-weighted national marginal social costs (mortalities/ktonne)


# Libraries ---------------------------------------------------------------
library(tidyverse)


# Directories -------------------------------------------------------------
damage_dir <- paste0(getwd(), '/results/social_cost/')
input_dir <- paste0(getwd(), '/inputs/')
# -------------------------------------------------------------------------
RCM_damages <- list.files(path=damage_dir, full.names=TRUE, pattern = '.csv') %>% 
  {data.table::rbindlist(lapply(., read_csv), use.names = T)} %>% 
  mutate(height = ifelse(eff_height == 220, 'elevated', 'ground'))  %>% 
  mutate(damage = mort_pertonne*1000, #mortalities/ktonne
         poll = ifelse(poll == 'PM25-PRI', 'primary PM2.5', poll)
  )

mean_damage <- RCM_damages %>% 
  group_by(poll, height, country) %>% 
  summarise(mean_damage = weighted.mean(x = damage, w = emis)) 


# # deaths using MSC
RCM_damages %>%
  mutate(PM_deaths = mort_pertonne*emis) %>%
  group_by(country) %>%
  summarise(sum(PM_deaths))
# 


# # deaths using C-R only
base_PMdeaths <- read_csv(paste0(getwd(), '/GBD_mortality/results/basePM_mort.csv')) %>%
  rename(basePM_mort = PM_mort
  ) %>%
  group_by(country) %>%
  summarise(sum(basePM_mort))
