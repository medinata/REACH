# Description -------------------------------------------------------------
#' Get GBD2019 PM2.5 and relative risk relationship for COPD, DM, LRI, LC, stroke, and IHD

#' Source:
#' https://zenodo.org/records/4642700
#' 
#' McDuffie, Erin E., Randall V. Martin, Joseph V. Spadaro, Richard Burnett, 
#' Steven J. Smith, Patrick O’Rourke, Melanie S. Hammer, et al. 2021. 
#' “Source Sector and Fuel Contributions to Ambient PM2.5 and Attributable
#'  Mortality across Multiple Spatial Scales.” Nature Communications 12 (1): 
#'  3594. https://doi.org/10.1038/s41467-021-23853-y.
#'  

#' Variables:
#' COPD: chronic obstructive pulmonary disease
#' DM: type II diabetes
#' LRI: lower respiratory infections
#' LC: lung cancer
#' IHD: ischemic heart disease
#' stroke


# Library -----------------------------------------------------------------
library(tidyverse)

# Directories -------------------------------------------------------------
mort_dir <- paste0(getwd(), '/GBD_mortality/')
input_dir <- paste0(mort_dir, 'inputs/GBD2019_BurdenData/')
open_input <- function(file_name) {
  read_csv(paste0(input_dir, file_name, '.csv'))
}

# GBD PM2.5 associated with RR --------------------------------------------
GBD_PM25 <- read_csv(paste0(input_dir, 'MRBRT_PM25.csv')) 
PM_conc <- GBD_PM25$`ug/m3`

# Age-specific causes -----------------------------------------------------
get_RR <- function(name_ext, disease_name) {
  open_input(paste0('MRBRT_',name_ext))%>% 
    select(ends_with('years')) %>% #ignore warning about other column names, 
    cbind(PM_conc) %>% 
    mutate(disease = disease_name)
}


stroke_RR <- get_RR('Stroke', 'stroke')

IHD_RR <- get_RR('IHD', 'IHD')


# Non age-specific causes -------------------------------------------------
diseases_1 <- c('COPD', 'DM', 'LRI', 'LC') #non-age specific


RR_list <- list()

for (ind in 1:length(diseases_1)) {
  RR_list[[ind]] <- read_csv(paste0(input_dir, 'MRBRT_', diseases_1[ind], '.csv'),
                             col_names = c(diseases_1[ind], '95_lower', '95_upper'),
                             skip =1) %>% 
    select(1)
}

GBD_RR <- do.call(cbind, RR_list) 


RR_curve <- cbind(GBD_RR, PM_conc = GBD_PM25$`ug/m3`) 









