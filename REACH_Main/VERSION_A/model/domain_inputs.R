# Description -------------------------------------------------------------
#' Input files specific to the domain

# Library -----------------------------------------------------------------
library(tidyverse)

# Directories -------------------------------------------------------------
open_dir <- function(name_ext) {
  paste0(getwd(),'/', name_ext, '/')
}

input_dir <- open_dir('inputs')

open_inputs <- function(input_name) {
  read_csv(paste0(input_dir,input_name))
}


# Inputs ------------------------------------------------------------------
# To estimate concentrations at the source location, the source location geography is
# divided into 100 smaller grids. This file contains sub-grids for every source 
# location and the corresponding centroids. 
sub_division <- open_inputs("simple_grid_2017.csv") %>%
  as.data.frame() 

# Centroids for receptor locations
receptors <- open_inputs("county_2017.csv") %>%
  as.data.frame() 

# Centroids for emission source locations
if (source_type %in% c('ground','elevated')) {
  emis_source <- receptors
}

if (source_type %in% c('egu','nonegu')) {
  emis_source <- read_csv(paste0(emis_dir, 'NEI_2017_',poll_var,'_nofires.csv')) %>% 
    filter(sector == source_type) %>% 
    as.data.frame()
}

# n: number of receptor locations, ID: location identifier
Input <- open_inputs("Input.csv") %>% 
  mutate(n = nrow(receptors))