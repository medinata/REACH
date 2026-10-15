
# Library -----------------------------------------------------------------
library(tidyverse)
library(sf)
# Directories -------------------------------------------------------------
input_dir <- paste0(getwd(),'/inputs/')
output_dir <- paste0(getwd(), '/policy_AQ')
map_dir <- paste0(input_dir, 'shapefile/')


# CHANGE SCENARIO NAMES ---------------------------------------------------
scenario_names <- c('scenario1', 'scenario2')


# Domain locations --------------------------------------------------------
domain_name <- 'southern_africa'
Divisions <- read_csv(paste0(input_dir,domain_name, ".csv")) %>%
  as.data.frame()

# Specify country for plot
country_name <- 'SA' #South Africa

division_sf <- st_read(paste0(map_dir, domain_name, ".shp")) %>% 
  filter(country == country_name)


# Folders -----------------------------------------------------------------
# create folders for each scenario
for (id in 1:length(scenario_names)) {
  # emissions directory
  dir.create(paste0(output_dir, '/new_emis/', scenario_names[id], '/'))

  # PM2.5 concentration directory
  dir.create(paste0(output_dir, '/results/PM25_concentration/', scenario_names[id], '/'))
  
  # Mortality directory
  dir.create(paste0(output_dir, '/results/mortality/', scenario_names[id], '/'))
}


# Run scripts -------------------------------------------------------------
run_script <- function(script_name) {
  source(paste0(output_dir, '/',script_name,'.R'), local = new.env())
}
run_script('00_emis_difference')
run_script('01_PM25_conc')
run_script('02_plot_conc')

