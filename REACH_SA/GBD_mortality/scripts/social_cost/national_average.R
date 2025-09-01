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


