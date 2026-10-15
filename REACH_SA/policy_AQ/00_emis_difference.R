# Description -------------------------------------------------------------
# Save emission totals after subtracting out policy emissions


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)


# Directories -------------------------------------------------------------
emis_dir <- paste0(getwd(), '/inputs/emissions/processed_emissions/')
scenario_dir <- paste0(getwd(), '/policy_AQ/scenario_emis/') 
export_dir <- paste0(getwd(), '/policy_AQ/new_emis/') 


# -------------------------------------------------------------------------
all_emis <- list.files(emis_dir, full.names = T, pattern = '.csv') %>% 
            lapply(read_csv) %>% 
            do.call(rbind, .) %>% 
            filter(!(poll %in% c('BC', 'OC')
                     )
                   ) %>% 
  mutate(height = ifelse(eff_height == 220, 'elevated', 'ground'),
         poll = case_when(poll == 'NMVOC' ~ 'VOC',
                          poll == 'PM2.5' ~ 'PM25-PRI',
                          TRUE ~ poll
                            )
         )
# 
# emis_template <- all_emis %>%
#   filter(country == 'SA' & height == 'elevated') %>%
#   mutate(emis = 0) %>%
#   select(c(1,6,7,9))
# 
# emis_template %>%
#   mutate(scenario = 'scenario1') %>%
# writexl::write_xlsx(paste0(scenario_dir, "scenario1.xlsx")
#                     )
# emis_template %>%
#   mutate(scenario = 'scenario2') %>%
# writexl::write_xlsx(paste0(scenario_dir, "scenario2.xlsx")
# )

poll_var <- unique(all_emis$poll)

# function to export emissions
get_emis <- function(height_var) {
  poll_emis <- new_emis %>% 
    filter(poll == poll_var[poll_id] & height == height_var) %>% 
    group_by(poll, height, census_id) %>% 
    summarise(emis = sum(emis))
  
  write_csv(poll_emis, file = paste0(emis_path, poll_var[poll_id], '_', height_var, '.csv'))
  
}
# -------------------------------------------------------------------------

for (k in 1:length(scenario_names)) {
  # open scenario emissions
  scenario_emis <- read_excel(paste0(scenario_dir, scenario_names[k], '.xlsx')) %>% 
    rename(policy_emis = emis) 
  
  # export directory for new emission totals 
  emis_path <- paste0(export_dir, scenario_names[k] ,'/')
  
  # subtract out emissions from each policy scenario
  new_emis <- all_emis %>% 
    rename(base_emis = emis) %>% 
    left_join(scenario_emis, by = c('census_id', 'poll', 'height')) %>% 
    mutate(emis = ifelse(!is.na(policy_emis), base_emis - policy_emis, base_emis
                         )
           )
  
  # check emissions difference
  new_emis %>% group_by(poll) %>% summarise(sum(base_emis) - sum(emis))
  
  # export emissions
  for (poll_id in 1:length(poll_var)) {
    
    get_emis('ground')
    
    get_emis('elevated')
    
  }
  
  # reset scenario emissions after each iteration
  rm(scenario_emis) 
}




