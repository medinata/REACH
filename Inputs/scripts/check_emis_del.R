library(tidyverse)
library(readxl)
library(data.table)

input_dir <- paste0(getwd(), '/files/emissions/')


emis_dir <- paste0(input_dir, 'edgar_totals/')
report_dir <- "G:/My Drive/Project/SA_region/inputs/old_edgar/summaries/"

all_emis <- list.files(emis_dir, full.names = T) %>% 
  lapply(fread) %>% 
  do.call(rbind, .)

regrid_emis <- all_emis %>% 
  group_by(country, poll) %>% 
 summarise(emis_regrid = sum(emis)) %>% 
  mutate(country = case_when(country == 'SA' ~ 'South Africa',
                             country == 'swz' ~ 'Swaziland',
                             TRUE ~ country))



poll_var <- c('PM2.5','SO2','NOx','NH3','NMVOC','OC','BC')

ipcc_list <- list()

for (i in 1:length(poll_var)) {
  ipcc_list[[i]] <- read_excel(paste0(report_dir,'v61_AP_',poll_var[i],'_1970_2018.xlsx'), 
                          sheet = 2, skip = 9) %>% 
    as.data.frame() %>% 
    filter(C_group_IM24_sh == 'Southern_Africa') %>% 
    filter(!Name == 'Tanzania_United Republic of') %>% 
    select(c(1:6,56))  %>% 
    rename(country = Name) %>% 
    group_by(country) %>% 
    summarise(emis = 1000*sum(Y_2018, na.rm = TRUE)) %>%   #Gg to metric tons
    mutate(poll = poll_var[i])

  
  
}

ipcc_emis <- do.call(rbind, ipcc_list)

compare_emis <- left_join(ipcc_emis, regrid_emis)  %>% 
  mutate(diff = emis_regrid - emis,
         per_diff = 100*(abs(diff)/(0.5*(emis + emis_regrid)))
  )
