poll_var <- "NOX"
q <- 3.17E4 # NOX emission rate in ug/s
t_loss <- 1.1*86400 # combined NOX oxidation and deposition timescale
t_chem <- 2.1*86400 # oxidation timescale
t_nitrate <- 0.95*86400 # total nitrate deposition 

gauss_plume <- function(met) {
  mutate(met, wind_coeff = (1/mean_SPD)*freq*((2*q)/sqrt(2*pi)),
         int_conc = wind_coeff*(s_z*((2*pi*X)/n))^-1*exp(-0.5*(eff_height/s_z)^2),
         conc = 1.37*int_conc/(t_chem*((1/t_loss) -(1/t_nitrate)))*(exp(-t/t_nitrate) - exp(-t/t_loss))
  )
}