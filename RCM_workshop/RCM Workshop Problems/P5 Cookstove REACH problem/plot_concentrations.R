library(viridis)
library(sf)
library(ggplot2)

#' shapefile: variable name of shapefile
#' df_name: variable name of data frame that contains concentrations
#' fill_name: name of PM2.5 concentration variable in quotations
#' export_dir: variable name of directory to save data 
#' export_name: name of file to export in quotations 

plot_PM25 <- function(df_name, shapefile, fill_name, export_dir, export_name) {
  PM25_conc <- left_join(shapefile, df_name, by = 'census_id')
  
  p <- ggplot() +
    geom_sf(data = PM25_conc, aes(fill = get(fill_name)), size = 0.1) +
    scale_fill_viridis(trans = 'log10') +
    labs(fill= ~ 'PM2.5 '(mu * g / m^3)) +
    theme_void() +
    theme(legend.title = element_text(size = 20)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.key.size = unit(1.5, 'cm')) +
    ggtitle(fill_name)
  
  ggsave(filename = paste0(export_name, '.png'), 
         plot = p, path = export_dir, height=7, width = 8.5, units = "in")
  
}
