# Description -------------------------------------------------------------
#' Compare EDGAR country totals after regridding


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Directories -------------------------------------------------------------
input_dir <- paste0(getwd(), '/files/')
sf_dir <- paste0(input_dir, 'shapefiles/')
emis_dir <- paste0(input_dir, 'emissions/emission_files/')
edgar_dir <- paste0(input_dir, 'emissions/edgar_summary/')
nc_dir <-  paste0(input_dir, 'emissions/nc_files/')


# Re-gridded totals -------------------------------------------------------

all_emis <- data.frame(emis_file = list.files(path = nc_dir)) %>% 
  mutate(code = gsub(".*_2022_\\s*|_flx.nc.*", "", emis_file),
         poll = gsub("v8.1_FT2022_AP_*|_2022.*", "", emis_file),
         export_name = paste0(poll, '_', code, '_2022'))

emis_list <- list()

for (j in 1:nrow(all_emis)) {
  emis_list[[j]] <- read_csv(paste0(emis_dir, all_emis$export_name[j], '.csv')) %>% 
    group_by(country, code, poll) %>% 
    summarise(emis_tot = sum(emis))
}

emis_df <- do.call(rbind, emis_list)

regrid_sum <-  emis_df %>% 
  group_by(country, poll) %>% 
  summarise(regrid_tot = sum(emis_tot)/1000)  %>% #tonnes to Gg
  mutate(country = case_when(country == 'SA' ~ 'South Africa',
                             country == 'swz' ~ 'Swaziland',
                             TRUE ~ country
  ))

# Compare totals ----------------------------------------------------------

countries <- unique(regrid_sum$country)

edgar_summary <- list.files(path = edgar_dir,
                            full.names = T)  %>% 
  lapply(function(file) {read_excel(file, skip = 9) 
  }) %>%
  bind_rows() %>% 
  dplyr::select(c(1:8,61)) %>% 
  filter(Name %in% countries) %>% 
  rename(poll = Substance, country = Name, emis = 9)



compare_totals <- edgar_summary %>% 
  group_by(poll, country) %>% 
  summarise(edgar_tot = sum(emis, na.rm = T)) %>% 
  left_join(regrid_sum, by = c('poll', 'country')) %>% 
  mutate(per_diff = 100*abs(edgar_tot - regrid_tot)/(0.5*(edgar_tot + regrid_tot
                                                          ))
  )

regional_totals <- compare_totals %>% 
  group_by(poll) %>% 
  summarise(edgar_tot = sum(edgar_tot), regrid_tot = sum(regrid_tot)) %>% 
  mutate(per_diff = 100*abs(edgar_tot - regrid_tot)/(0.5*(edgar_tot + regrid_tot
  ))
  )
