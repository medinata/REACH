
evaluate_model <- function(var_name) {
  
  comp_conc <- filter(observations, species == var_name) %>% 
    left_join(., final_conc[,c('GEOID17',var_name,'Longitude')]) 
  
  
  colnames(comp_conc) [5] <- "model"
  
  comp_conc <- filter(comp_conc, !is.na(model))
  
  
  max_PM <- max(comp_conc[,c("observations","model")])
 
  
  a <- ggplot(comp_conc, aes(x=observations, y=model)) +
    geom_point(aes(color = Longitude, shape = network), size = 3) +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(intercept = 0, slope = 2, color = 'darkgrey') +
    geom_abline(intercept = 0, slope = 0.5, color = 'darkgrey') +
    scale_color_viridis() +
    geom_abline(intercept = 0, slope = 1) +
    theme(aspect.ratio=1) +
    scale_x_continuous(labels = function(x) format(x, scientific = FALSE), limits = c(0,max_PM)) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE), limits = c(0,max_PM)) +
    stat_cor(
      aes(label = paste(..rr.label..)),
      label.x = 0, size = 7, 
    )   +
    theme(text = element_text(size=25))  +
    ylab(~ "model " (mu*g/m^3)) +
    xlab(~ "observations " (mu*g/m^3)) +
    theme(legend.position="right") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) 
  
  ggsave(paste0(plot_dir,var_name,'.png'),width = 11, height = 8,a)

}


