poll_var <- "NH3"
q <- 3.17E4
t_loss <- 2.3*86400

gauss_plume <- function(met) {
  mutate(met, wind_coeff = (1/mean_SPD)*freq*((2*q)/sqrt(2*pi)),
         int_conc = wind_coeff*(s_z*((2*pi*X)/n))^-1*exp(-0.5*(eff_height/s_z)^2),
         conc = int_conc*exp(-t/t_loss))
}