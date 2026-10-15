
policy_dir <- paste0(getwd(), '/policy_AQ/')
conc_dir <- paste0(policy_dir, 'results/PM25_concentration/')
plot_dir <- paste0(policy_dir, 'results/plots/')

for (k in 1:length(scenario_names)) {
  pm_dir <- paste0(conc_dir, scenario_names[k], '/')
  
  pm_conc <-  read_csv(paste0(pm_dir, 'pm_conc.csv')) %>% 
    select(census_id, PM_diff)
  
  pm_sf <- left_join(division_sf, pm_conc) 
  
  plot_data <-  ggplot(data = pm_sf) +
    geom_sf(aes(fill = PM_diff), color=NA, size = 0.2) +
    scale_fill_gradientn(
      colours = c("lightyellow", "orange", "red", "darkred"),
      name = expression(PM[2.5]~(mu*g/m^3))
    )+
    labs(fill = ~ "PM2.5 " (mu*g/m^3))  +
    theme(legend.key.size = unit(2, 'cm'),
          legend.title = element_text(size = 26),
          legend.text = element_text(size = 22),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle( scenario_names[k])
  
  ggsave(paste0(plot_dir,scenario_names[k],'.png'),width = 11, height = 8,plot_data,dpi = 600)
  
  
  
  
    
}
