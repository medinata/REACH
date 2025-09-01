


# Libraries ---------------------------------------------------------------
library(tidyverse)

# Directories -------------------------------------------------------------
GBD_dir <- paste0(getwd(), '/GBD_mortality/inputs/GBD2019_BurdenData/')
mort_dir <- paste0(getwd(), '/GBD_mortality/inputs/')

file_ext <- 'GBD19_2019_Baseline_Mortality_'

causes <- c("COPD", "DM", "LRI", "LC", "Stroke", "IHD")

mort_list <- list()

for (j in 1:length(causes)) {
  mort_list[[j]] <- read_csv(paste0(GBD_dir, file_ext, causes[j], '.csv')) %>% 
    select(1,8:21) %>% 
    mutate(disease = causes[j])
}


countries <- c('Angola', 'Botswana', 'Eswatini', 'Lesotho', 'Malawi', 'Mozambique', 'Namibia', 
               'South Africa','Zambia','Zimbabwe')

mort_df <- do.call(rbind, mort_list) %>% 
  filter(Location %in% countries) %>% 
  pivot_longer(c(2:15), values_to = 'deaths', names_to = 'age_group')

deaths_30plus <- mort_df %>% 
  group_by(Location, disease) %>% 
  summarise(tot_deaths = sum(deaths))


check_mort <- read_csv(paste0(mort_dir, 'sa_mortality_cause_specific.csv')) %>% 
  filter(!is.na(cause_name)) %>%
  group_by(country, cause_name) %>% 
  summarise(tot_deaths = sum(deaths))
  
