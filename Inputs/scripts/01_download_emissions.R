# Description -------------------------------------------------------------
#' Download emissions from EDGARv6.1
# https://edgar.jrc.ec.europa.eu/dataset_ap61

# Libraries ---------------------------------------------------------------
library(tidyverse)


# Directories -------------------------------------------------------------
input_dir <- paste0(getwd(), '/files/emissions/')
emission_zipdir <- paste0(input_dir, 'zip_files/')
info_dir <- paste0(input_dir, 'info/')
export_dir <- paste0(input_dir, 'nc_files')
# -------------------------------------------------------------------------

#' This file was created based on the edgar version listed for the pollutants
#' of interest. Check and adjust sector and pollutant names for your purpose
sector <- read_csv(paste0(info_dir, 'edgar_sectors.csv'))

base_url <- 'https://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/'
edgar_dataset <- 'v61_AP'
edgar_version <- 'EDGARv6.1'
emis_year <- '2018'
end_url <- '.0.1x0.1.zip'

url_list <- list()
names_list <- list()


# Create urls to download each file ---------------------------------------

for (j in 1:nrow(sector)) {
  sector_poll <- str_split(sector$poll[j], ",")[[1]]
  
  names_list[[j]] <- paste0(edgar_version, '_', sector_poll, '_', 
                            emis_year, '_', sector$code[j], '.zip')
  
  url_list[[j]] <- paste0(base_url, edgar_dataset, '/', sector_poll, '/', sector$code[j],
         '/', edgar_version, '_', sector_poll, '_', emis_year, '_', sector$code[j],
         end_url)
}


# Save zip files to directory ---------------------------------------------
emis_files <- as.list(unlist(url_list))
export_names <- as.list(unlist(names_list))


save_zip <- function(url_string, export_string) {
  download.file(url = url_string, 
                destfile = paste0(emission_zipdir, export_string))
}


map2(.x = emis_files, .y = export_names, .f = save_zip)



# Unzip nc files ----------------------------------------------------------
zipF <- list.files(path = emission_zipdir, pattern = "*.zip", full.names = TRUE)



for(file_j in 1:length(zipF)) {
  
  zipped_nc_names <- grep('\\.nc$', unzip(zipF[file_j], list=TRUE)$Name, 
                          ignore.case=TRUE, value=TRUE)
  
  
  unzip(zipfile = zipF[file_j], 
        files = zipped_nc_names,
        exdir =  export_dir)
  
}



