# Description -------------------------------------------------------------
#' Get GBD2019 RR input data from each age group and disease based on McDuffie concentration estimate
#' Source:
#' https://zenodo.org/records/4642700


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Directories -------------------------------------------------------------
mort_dir <- paste0(getwd(), '/GBD_mortality/')
input_dir <- paste0(mort_dir, 'inputs/GBD2019_BurdenData/')
open_input <- function(file_name) {
  read_csv(paste0(input_dir, file_name, '.csv'))
}

plot_dir <- paste0(mort_dir, 'plots/')
conc_dir <- paste0(mort_dir, 'mcduffie_2021/')
#conc_dir <- paste0(getwd(), '/results/concentrations/')
#export_dir <- paste0(mort_dir, '/results/REACH_RR/')


# PM2.5 estimates ---------------------------------------------------------
countries <- c('Angola', 'Botswana', 'Eswatini', 'Lesotho', 'Malawi', 'Mozambique', 'Namibia', 
               'South_Africa','Zambia','Zimbabwe')

# paper_PM25 <- read_excel(paste0(conc_dir, 'mortalities.xlsx'), skip = 6) %>% 
#   select(country = 1, PM25 = 2) %>% 
#   filter(country %in% countries) %>% 
#   mutate(base_PM25 = as.numeric(PM25))

paper_PM25 <- read_excel(paste0(conc_dir, 'mortalities.xlsx'), skip = 6) %>% 
  select(country = 1, PM25 = 2) %>% 
  filter(country %in% countries) %>% 
  mutate(base_PM25 = as.numeric(PM25))


#max_PM <- max(paper_PM25$base_PM25)

# GBD PM2.5 associated with RR --------------------------------------------
GBD_PM25 <- read_csv(paste0(input_dir, 'MRBRT_PM25.csv')) 
PM_conc <- GBD_PM25$`ug/m3`


# Age-specific causes -----------------------------------------------------
get_RR <- function(disease_name) {
  open_input(paste0('MRBRT_',disease_name))%>% 
    select(ends_with('years')) %>% 
    cbind(PM_conc) %>% 
    #pivot_longer(c(1:15), names_to = 'age_group', values_to = 'RR') %>% 
    mutate(disease = disease_name)
}


stroke_RR <- get_RR('stroke')

IHD_RR <- get_RR('IHD')




# Non age-specific causes -------------------------------------------------
diseases_1 <- c('COPD', 'DM', 'LRI', 'LC') #non age-specific


RR_list <- list()

for (ind in 1:length(diseases_1)) {
  RR_list[[ind]] <- read_csv(paste0(input_dir, 'MRBRT_', diseases_1[ind], '.csv')) %>% 
    select(1)
}

GBD_RR <- do.call(cbind, RR_list) 


RR_curve <- cbind(GBD_RR, PM_conc = GBD_PM25$`ug/m3`) 


# -------------------------------------------------------------------------
# p1 <- ggplot(RR_curve %>%  filter(PM_conc < 100), aes(x = PM_conc, y = RR, colour = disease)) +
#   geom_point()
# 
# 
# ggsave(paste0(plot_dir, 'RR_curve.png'), width = 15, height = 8, p1, dpi = 500)


# Interpolate RR for non-age specific causes ------------------------------



interpolate_list <- list()

for (j in 1:length(diseases_1)) {
  cause_RR <- RR_curve %>% 
    select(PM_conc, RR = diseases_1[j]) 
  
  interpolate_list[[j]] <- paper_PM25 %>% 
    mutate(RR =  spline(x = cause_RR$PM_conc, y = cause_RR$RR, xout = base_PM25)$y,
           cause = diseases_1[j]
    )
}

paper_otherRR <- do.call(rbind, interpolate_list) %>% 
  mutate(PAF = 1 - (1/RR),
         country = ifelse(country == 'South_Africa', 'South Africa', country))


# check interpolated RR against GBD curve ---------------------------------
# GBD_RR <- RR_curve %>% 
#   pivot_longer(c(1:4), names_to = 'cause', values_to = 'RR')
# 
# p2 <- ggplot() +
#   geom_point(data = GBD_RR %>% filter(PM_conc <= max_PM), aes(x = PM_conc, y =RR, group = cause)) +
#   geom_point(data = paper_otherRR , aes(x = base_PM25, y = RR, colour = cause)) 
# 
# 
# ggsave(paste0(plot_dir, 'paper_otherRR.png'), width = 15, height = 8, p2, dpi = 500)


# Interpolate RR for age-specific causes ----------------------------------
age_groups <- colnames(stroke_RR)[1:15]

other_RR <- rbind(stroke_RR, IHD_RR)
diseases_2 <- c('stroke', 'IHD')


obtain_RR <- function(cause_name) {
  RR_list <- list()
  
  for (age_ind in 1:length(age_groups)) {
    RR_info <- other_RR %>% 
      filter(disease == cause_name) %>% 
      select(PM_conc, age_groups[age_ind]) %>% 
      rename(RR = 2)
    
    RR_list[[age_ind]] <- paper_PM25 %>% 
      mutate(RR =  spline(x = RR_info$PM_conc, y = RR_info$RR, xout = base_PM25)$y,
             cause = cause_name,
             age_group = age_groups[age_ind],
             age_group = gsub("([0-9]+)-([0-9]+) years", "\\1 to \\2", age_group),
             age_group = ifelse(age_group == "95+ years", "95 plus", age_group),
             PAF = 1 - (1/RR),
             country = ifelse(country == 'South_Africa', 'South Africa', country)
      )
    
  }
  
  do.call(rbind, RR_list) %>% 
    select(c(1,6,7)) 
}

paper_strokeRR <- obtain_RR('stroke')
paper_IHDRR <- obtain_RR('IHD')


# PM2.5 mortalities -------------------------------------------------------

file_ext <- 'GBD19_2017_Baseline_Mortality_'


causes <- c("COPD", "DM", "LRI", "LC", "Stroke", "IHD")

mort_list <- list()

for (j in 1:length(causes)) {
  mort_list[[j]] <- read_csv(paste0(input_dir, file_ext, causes[j], '.csv')) %>% 
    select(country = 1,7:21,mort_over25 = `over 25`) %>% 
    mutate(cause = causes[j])
}

countries <- c('Angola', 'Botswana', 'Eswatini', 'Lesotho', 'Malawi', 'Mozambique', 'Namibia', 
               'South Africa','Zambia','Zimbabwe')

mort_df <- do.call(rbind, mort_list) %>% 
  filter(country %in% countries)


PM25_other <- paper_otherRR %>% 
  left_join(mort_df, by = c('country', 'cause')) %>% 
  mutate(PM_deaths = PAF * mort_over25) %>% 
  group_by(country, cause) %>% 
  summarise(tot_deaths = sum(PM_deaths))


PM25_stroke <- mort_df %>% 
  filter(cause == 'Stroke') %>% 
  pivot_longer(c(2:16), names_to = 'age_group', values_to = 'mort') %>% 
  left_join(paper_strokeRR, by = c('age_group', 'country')) %>% 
  mutate(PM_deaths = PAF * mort)%>% 
  group_by(country, cause) %>% 
  summarise(tot_deaths = sum(PM_deaths))


PM25_IHD <- mort_df %>% 
  filter(cause == 'IHD') %>% 
  pivot_longer(c(2:16), names_to = 'age_group', values_to = 'mort') %>% 
  left_join(paper_IHDRR, by = c('age_group', 'country')) %>% 
  mutate(PM_deaths = PAF * mort) %>% 
  group_by(country, cause) %>% 
  summarise(tot_deaths = sum(PM_deaths))


PM25_deaths <- rbind(PM25_IHD, PM25_other, PM25_stroke)


adj_factors <- read_csv(paste0(mort_dir, '/inputs/GBD2019_BurdenData/CoExposure_Factors.csv')) %>% 
  select(country = 1, adj = `2017`) %>% 
  filter(country %in% countries) 


mcduffie <- PM25_deaths %>% 
  group_by(country) %>% 
  summarise(tot_deaths = sum(tot_deaths)) %>% 
  mutate(source = 'McDuffie') 




reach_estimate <- read_csv('reach_deaths_ages30plus.csv') %>% 
  rename(tot_deaths = 2) %>% 
  mutate(source = 'REACH',
         country = case_when(country == 'SA' ~ 'South Africa',
                             country == 'swz' ~ 'Eswatini',
                             TRUE ~ country))

compare_deaths <- rbind(mcduffie, reach_estimate) %>% 
  pivot_wider(names_from = source, values_from = tot_deaths) %>% 
  left_join(adj_factors) %>% 
  mutate(McDuffie_adj = adj*McDuffie,
    diff = McDuffie - REACH,
    diff_2 = McDuffie_adj - REACH) 

# export_files <- function(df, save_name) {
#   write_csv(df, paste0(export_dir, save_name, '.csv'))
# }
# 
# export_files(paper_otherRR, 'other_causes')
# export_files(paper_strokeRR, 'Stroke')
# export_files(paper_IHDRR, 'IHD')






# DELETE
# 
# otherRR_list <- list()
# 
# for (dis_ind in 1:length(diseases_2)) {
#   RR_list <- list()
#   
#   for (age_ind in 1:length(age_groups)) {
#     RR_info <- other_RR %>% 
#       filter(disease == diseases_2[dis_ind]) %>% 
#       select(PM_conc, age_groups[age_ind]) %>% 
#       rename(RR = 2)
#     
#     RR_list[[age_ind]] <- paper_PM25 %>% 
#       mutate(RR =  spline(x = RR_info$PM_conc, y = RR_info$RR, xout = base_PM25)$y,
#              cause = diseases_2[dis_ind],
#              age_group = age_group[ind]
#       )
#     
#     
#   }
#   
#   otherRR_list[[dis_ind]] <- do.call(rbind, RR_list)
# }
# 
# interRR_group2 <- do.call(rbind, otherRR_list)




















# -------------------------------------------------------------------------


max_PM <- max(paper_PM25$base_PM25)

# add condition, if less than or equal to counterfactual, set RR = 1
test_RR <- RR_curve %>% select(COPD, PM_conc) %>% 
  rename(RR = 1)
pm_target <- paper_PM25$base_PM25[1]
rr_interpolate <- spline(x = test_RR$PM_conc, y = test_RR$RR, xout = pm_target)$y
if (pm_target <= 2.4) {
  rr_interpolate <- 1
}
PAF_test <- 1-(1/rr_interpolate)

interpolated_RR_test <- paper_PM25 %>% 
  mutate(RR =  spline(x = test_RR$PM_conc, y = test_RR$RR, xout = base_PM25)$y)

p2 <- ggplot() +
  geom_point(data = test_RR %>% filter(PM_conc <= max_PM), aes(x = PM_conc, y = RR), color = 'grey') +
  geom_point(data = interpolated_RR, aes(x = base_PM25, y = RR), color = 'red') 



ggsave(paste0(plot_dir, 'check_interpolate.png'), width = 15, height = 8, p2, dpi = 500)

