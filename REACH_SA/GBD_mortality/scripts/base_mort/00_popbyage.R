# Description -------------------------------------------------------------



# Library -----------------------------------------------------------------
library(tidyverse)
library(readxl)


# Directories -------------------------------------------------------------
open_dir <- function(name_ext) {
  paste0(getwd(),'/inputs/', name_ext, '/')
}

pop1_dir <- open_dir('population/uscensus_proj/')
pop2_dir <- open_dir('population/')
input_dir <- paste0(getwd(),'/inputs/')
# References --------------------------------------------------------------


# Inputs ------------------------------------------------------------------
max_adm <- read_excel(paste0(input_dir, 'sa_admn.xlsx'))

sa_info <- read_csv(paste0(input_dir, 'southern_africa.csv'))


age_min <- 25# populations with persons of ages 25 +
# South Africa population -------------------------------------------------
new_df <- sa_info %>%
  filter(country == 'South Africa')  %>%
  separate(col = geo_name, into = c("ADM3_NAME", "id"), sep = " (?=[^ ]*$)\\(",
           remove = FALSE) %>%
  separate(col = id, into = c("Loc_Code", NA), sep = "\\)")


metro_municip <- read_excel(paste0(pop2_dir,
                                   'DistrictCouncilprojectionbysexandage(2002-2021).xlsx'), skip = 4) %>% 
  filter(grepl('Metropolitan', Name)) %>% 
  select(c(1:3, pop = '2018'))


municip_code <- data.frame(Name = unique(metro_municip$Name),
                           Loc_Code = c('CPT', 'BUF', 'NMA',
                                        'MAN', 'ETH', 'EKU',
                                        'JHB', 'TSH')) %>% 
  left_join(new_df[,c('ADM3_NAME', 'Loc_Code')])


metro_municip %<>% 
  left_join(municip_code, by = 'Name') %>% 
  group_by(Loc_Code, Age) %>% 
  summarise(pop = sum(pop))


local_municip <- read_excel(paste0(pop2_dir, '2018_southafrica_localmunicip.xlsx')) %>% 
  select(c(1:6, pop = '2018')) %>% 
  group_by(Loc_Code, Age) %>% 
  summarise(pop = sum(pop)) 



all_municip <- rbind(metro_municip, local_municip) %>%
  mutate(Age = case_when(Age == '80+' ~ '80-NA',
                         Age == '75+' ~ '75-NA',
                         TRUE ~ Age),
         age_group = str_replace(Age, "(\\d+)\\s*-\\s*(\\d+)", "B\\1\\2_2018"),
         #age_group = ifelse(age_group == '80+', 'B80PL_2018', age_group),
         age_group = case_when(Age == '80-NA' ~ 'B80PL_2018',
                               Age == '75-NA' ~ 'B75PL_2018',
                               TRUE ~ age_group
                               )
         ) %>% 
  separate(Age,into = c('start_age',NA), sep = '-') %>% 
  mutate_at(vars(start_age), list(as.numeric)) %>% 
  filter(start_age >= age_min)

sa_pop <-  all_municip %>% 
  left_join(new_df) %>% 
  ungroup() %>% 
  select(geo_name, age_group, pop) %>% 
  mutate(country = 'SOUTH AFRICA') 

# Eswatini population -----------------------------------------------------
# 2018 population

#' Figured out name differences between swz_dist and sa_info through 
#' what region the sub district is in, this source and a quick Google search
# https://www.gov.sz/index.php/learn-about-your-inkhundla
name_diff <- read_csv(paste0(pop2_dir, 'swz_new_names.csv')) 

#' Population of Eswatini
age_dist <- read_excel(paste0(pop2_dir,'2018_swz_population.xlsx')) %>% 
  filter(GROUP != 'TOTAL') %>% 
  mutate(age_group = str_replace(GROUP, "(\\d+)\\s*-\\s*(\\d+)", "B\\1\\2_2018"),
         GROUP = ifelse(GROUP == '100+', '100 - NA', GROUP)) %>% #CHANGE TO NUMERIC
  separate(GROUP,into = c('start_age',NA), sep = '-') %>%
  mutate_at(vars(start_age), list(as.numeric)) %>% 
  mutate(age_group = ifelse(start_age >= 80, 'B80PL_2018', age_group)) %>% 
  filter(start_age >= age_min)


pop_byage <- age_dist %>% 
  group_by(age_group) %>% 
  summarise(pop_tot = sum(Population))


# 2007 population
#' Use 2007 population distribution by tinkhundla 
#' Determine fraction of total population in each tinkundla
#' Multiply by National age distribution (total and for ages 25 and above)
swz_dist <- read_excel(paste0(pop2_dir,'2007_swz_Tinkhundla_population.xlsx'),
                       range = 'A14:O72') %>% 
  dplyr::select(c(geo_name = 2,pop_2007 = 7)) %>% 
  filter(!is.na(geo_name)) %>% 
  left_join(name_diff) %>% 
  mutate(pop_frac = pop_2007/sum(pop_2007), # national population distribution by sub-region (Tinkhundla)
         geo_name = ifelse(!is.na(new_name),new_name, geo_name)) %>% 
  select(geo_name, pop_frac) #Correct name differences between this file and sa_file


swz_pop <- expand.grid(geo_name = swz_dist$geo_name, age_group = pop_byage$age_group) %>% 
  left_join(pop_byage) %>% 
  left_join(swz_dist) %>% 
  mutate(pop = pop_frac*pop_tot,
         country = 'ESWATINI') %>% 
  select(1, 2,5,6)



# Other countries' population ---------------------------------------------
pop_files <- data.frame(files = list.files(pop1_dir, pattern = '.xlsx')) %>% 
  mutate(country = sub("\\_.*", "", files)) %>% 
  left_join(max_adm, by = 'country')

pop_list <- list()

for (j in 1:nrow(pop_files)) {
  pop_list[[j]] <- readxl::read_excel(paste0(pop1_dir, pop_files$files[j]),
                                      sheet = 3, skip = 3) %>% 
    filter(ADM_LEVEL == pop_files$ADM_level[j]) %>% 
    dplyr::select(c(geo_name = GEO_CONCAT,country = CNTRY_NAME, B2529_2018:B80PL_2018)) %>% 
    pivot_longer(cols = B2529_2018:B80PL_2018, #only consider populations with ages of persons 25 + for concentration-response function
                 values_to = 'pop', 
                 names_to = 'age_group')
  
}

other_pop <- do.call("rbind", pop_list) 

# Domain population by age ------------------------------------------------
pop_info <- rbind(sa_pop, swz_pop, other_pop) %>% 
  mutate(age_group = case_when(!age_group %in% c('B80PL_2018', 'B75PL_2018') ~ # format the same as population files
                                 str_replace_all(age_group, "^B(\\d{2})(\\d{2})_\\d{4}$", "\\1-\\2 years"),
                               age_group == 'B80PL_2018' ~ '80+ years',
                               age_group == 'B75PL_2018' ~ '75+ years'
                               )
  )


write_csv(pop_info, paste0(pop2_dir,'popbyage.csv'))

