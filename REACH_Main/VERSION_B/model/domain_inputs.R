# Description -------------------------------------------------------------
#' Input files specific to the domain

# Library -----------------------------------------------------------------
library(tidyverse)

# Directories -------------------------------------------------------------
open_dir <- function(name_ext) {
  paste0(getwd(),'/', name_ext, '/')
}

input_dir <- open_dir('inputs')
emis_dir <- open_dir('inputs/emissions/processed_emissions')


# Inputs ------------------------------------------------------------------
# To estimate concentrations at the source location, the source location geography is
# divided into 100 smaller grids. This file contains sub-grids for every source 
# location and the corresponding centroids. 
sub_division <- read_csv(paste0(input_dir,'sub_grids.csv'))

# Centroids for emission source locations
emis_source <- read_csv(paste0(emis_dir, poll_var, '_', source_type, '.csv')) %>% 
  as.data.frame()

# Centroids for receptor locations
receptors <- emis_source

# n: number of receptor locations, ID: location identifier
Input <- data.frame(n = nrow(receptors),
                    ID = 'census_id')

