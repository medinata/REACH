
# Library -----------------------------------------------------------------
library(tidyverse)
library(readxl)


# Directories -------------------------------------------------------------
mort_dir <- paste0(getwd(), '/GBD_mortality/')
conc_dir <- paste0(getwd(), '/results/concentrations/')
plot_dir <- paste0(mort_dir, 'plots/')

REACH_deaths <- read_csv(paste0(mort_dir, 'results/basePM_mort.csv')) %>%
  group_by(country) %>% 
  summarise(REACH_mort = sum(PM_mort))

# compare to McDuffie et al -----------------------------------------------
countries <- c('Angola', 'Botswana', 'Eswatini', 'Lesotho', 'Malawi', 'Mozambique', 'Namibia', 
               'South_Africa','Zambia','Zimbabwe')
mcduffie <- read_excel(paste0(mort_dir, '/mcduffie_2021/mortalities.xlsx'), skip = 6) %>% 
  select(country = 1, PM25 = 2, GBD_deaths = 23, GEMM_deaths = 26) %>% 
  filter(country %in% countries) %>% 
  mutate(PM_avg = as.numeric(PM25),
         GBD_deaths = as.numeric(GBD_deaths),
         country = ifelse(country == 'South_Africa', 'South Africa', country))

countries2 <- c('Angola', 'Botswana', 'Eswatini', 'Lesotho', 'Malawi', 'Mozambique', 'Namibia', 
                'South Africa','Zambia','Zimbabwe')

# REPLACE ADJUSTMENT FOR REACH with 2018 values
compare_deaths <- left_join(REACH_deaths, mcduffie)

#write_csv(adj_factors, paste0(mort_dir, 'sa_coexposure_factors.csv'))



# comparison plots --------------------------------------------------------
reach_pm <- read_csv(paste0(conc_dir, 'PM25_pop_weighted.csv')) %>% 
  mutate(source = 'REACH (2018 PM2.5)')

compare_pm <- mcduffie %>% select(country, PM_avg) %>% 
  mutate(source = 'McDuffie (2019 PM2.5)') %>% 
  rbind(reach_pm)

a <- ggplot(compare_pm, aes(x=country, y=PM_avg, fill=source)) +
  geom_bar(stat="identity", position = 'dodge') +
  theme(axis.text.x = element_text(size = 9, colour = "black"))  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab(~ "Pop-weighted PM2.5 " (mu*g/m^3)) +
  xlab('')



ggsave(filename = "compare_PM.png", 
       plot = a, path = plot_dir, height=7, width = 8.5, units = "in")



select_country <- c('South Africa')



# compare_deaths <- mcduffie %>% 
#   select(country, deaths = GBD_deaths) %>% 
#   mutate(source = 'McDuffie') %>% 
#   rbind(REACH_deaths)

compare_GBD <- compare_deaths %>% 
  select(country, GBD_deaths, REACH_mort) %>% 
  pivot_longer(2:3, names_to = 'source', values_to = 'deaths')

compare_GBD$Facet <- ifelse(compare_GBD$country %in% select_country, "group1", "group2")

b <- ggplot(compare_GBD, aes(x=country, y=deaths, fill=source)) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_wrap(~Facet, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 14),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 9),
        panel.background = element_blank()) +
  ylab(~ "PM2.5 Attributable Deaths ") +
  xlab('')


ggsave(filename = "compare_deaths.png", 
       plot = b, path = plot_dir, width = 15, height = 8, dpi = 500)



