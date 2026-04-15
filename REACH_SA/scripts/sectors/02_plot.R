# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(viridis)
library(RColorBrewer)
library(readxl)

# Directories -------------------------------------------------------------
curr_dir <- getwd()
conc_dir <- paste0(curr_dir, '/results/sector_conc/')
mort_dir <- paste0(curr_dir, '/results/sector_deaths/')
input_dir <- paste0(curr_dir, '/inputs/')
info_dir <- paste0(curr_dir, '/inputs/emissions/sector_emissions/')
plot_dir <- paste0(curr_dir, '/plots/paper/')

# -------------------------------------------------------------------------
domain_pop <- read_csv(paste0(input_dir, 'population/southern_africa_pop.csv')) %>% 
  select(census_id, pop_tot)

sector_info <- read_excel(paste0(info_dir, 'edgar_sectors.xlsx')) %>% 
  select(sector = code, sector_name = sector)

# Sector deaths -----------------------------------------------------------
# Plot PM2.5-attributed mortalities
all_deaths <- list.files(mort_dir, full.names = T, pattern = '.csv') %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  group_by(country, sector) %>% 
  summarise(sector_deaths = sum(sector_mort))



top_mort_sectors <- all_deaths %>%
  group_by(country) %>%
  arrange(country, desc(sector_deaths)) %>%
  slice_head(n = 7) 

plot_mort <- all_deaths %>%
  mutate(sector = ifelse(sector %in% unique(top_mort_sectors$sector), as.character(sector), 'Other')) %>% 
  left_join(sector_info) %>% 
  mutate(sector_name = ifelse(is.na(sector_name), sector, sector_name))


select_country <- c('South Africa', 'Angola', 'Zimbabwe')

plot_mort$Facet <- ifelse(plot_mort$country %in% select_country, "group1", "group2")

country_total <- plot_mort %>% 
  group_by(country) %>% 
  summarise(country_tot = sum(sector_deaths))


per_sector <- plot_mort %>% 
  group_by(sector_name, country) %>% 
  summarise(sector_tot = sum(sector_deaths)) %>% 
  left_join(country_total) %>% 
  mutate(per = sector_tot/country_tot*100) 


regional_mort <- plot_mort %>% 
  group_by(sector_name) %>% 
  summarise(sector_tot = sum(sector_deaths)) %>% 
  mutate(per = sector_tot/sum(sector_tot)*100) 


p1 <- ggplot(plot_mort, aes(x = country,  y = sector_deaths, fill = sector_name)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~Facet, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 18),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 22),
        axis.text.y = element_text(color = "black", size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        ) + 
  labs(y = 'Annual Deaths',
       fill = 'sector')

ggsave(paste0(plot_dir,'sector_PM25deaths.png'), width = 17, height = 8, p1, dpi = 600)


p2 <- ggplot(per_sector, aes(x = country, y = per, fill = sector_name)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 14),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14),
        axis.text.y = element_text(color = "black", size = 9)) + 
  labs(y = '% Mortalities',
       fill = 'sector')

p2


ggsave(paste0(plot_dir,'percent_PM25deaths.png'), width = 15, height = 8, p2, dpi = 500)



# Sector PM2.5 ------------------------------------------------------------
# Plot sector pop-weighted concentration by country
all_conc <- list.files(conc_dir, full.names = T, pattern = '.csv') %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  left_join(domain_pop)


country_pm <- all_conc %>% 
  group_by(country, sector) %>% 
  summarise(weighted_pm = weighted.mean(x = PM_sector, w = pop_tot)) 


plot_sectors <- country_pm %>%
  mutate(sector = ifelse(sector %in% unique(top_mort_sectors$sector), as.character(sector), 'Other')) %>% 
  left_join(sector_info) %>% 
  mutate(sector_name = ifelse(is.na(sector_name), sector, sector_name))

p3 <- ggplot(plot_sectors, aes(x = country, y = weighted_pm, fill = sector_name)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()  +
  labs(
    fill = 'sector',
    x = '',
    y = expression("Average PM"[2.5]~"(µg/m³)")
  ) +
  #scale_fill_manual(values = adjusted_palette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 14),  # Increase size of y-axis labels
        axis.title.x = element_text(size = 16),  # Increase size of x-axis title
        axis.title.y = element_text(size = 16),  # Increase size of y-axis title
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Increase title size
        strip.text = element_text(size = 14)  # Increase size of facet labels
  )


ggsave(paste0(plot_dir,'sector_PM25.png'), width = 15, height = 8, p3, dpi = 500)
