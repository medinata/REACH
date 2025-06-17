# pollutant specific ------------------------------------------------------

q <- 15854.9 # 1 mton SO2/yr in ug/s of Sulfur 
t_SO4_D <- 7*86400 #  sulfate deposition timescale (7 days) in seconds


gauss_plume <- function(met) {
  mutate(met, wind_coeff = (1/mean_SPD)*freq*((2*q)/sqrt(2*pi)),
         PM = wind_coeff*(s_z*((2*pi*X)/n))^-1*exp(-0.5*(eff_height/s_z)^2),
         PM_S = PM*exp(-t/t_SO2), # sulfur concentration
         SO2_conc = 2*PM_S,
         conc = ((3/(t_chem*((-1/t_SO2)+(1/t_SO4_D))))*(PM_S-(PM*exp(-t/t_SO4_D)))
         ))
}