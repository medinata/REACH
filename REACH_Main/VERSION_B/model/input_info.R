# Description -------------------------------------------------------------
#' Input data for Gaussian-plume dispersion calculations
#' Adjust variables and assumptions if needed


# Variable and function definition ----------------------------------------
#' ground_ht:  height of ground-level emission sources (m)
#' elevated_ht: height of elevated emission sources (m)
#' t: transport time (s)
#' X: distance downwind (m)
#' mean_SPD: mean wind speed (m/s)
#' s_z: vertical dispersion (m)
#' DIR: wind direction (degrees)
#' sec_val: assigned sector number based on wind direction
#' stab_class: atmospheric conditions for dispersion (Pasquill classes A-F)
#' neu_weight and stab_weight: Values applied to stable vertical dispersion parameters (class E and F)
#' to transition air masses from neutral to stable conditions as air masses don't remain stable indefinetely 
#' gauss_plume: function to estimate concentrations (dispersion and/or chemistry timescales
#' applied varies per pollutant, see dispersion folder)


# input parameters ---------------------------------------------------------------
ground_ht <- 10 
elevated_ht <- 220 

if (source_type == 'ground') {
  eff_height <- ground_ht
}

if (source_type == 'elevated') {
  eff_height <- elevated_ht
}


sigmaz_thresh <- 5000 # Threshold for vertical dispersion coefficient (5 km)
trans_begin <- 12 * 3600 # Start of transition time (sec) from stable to neutral stability
trans_end <- 36 * 3600 # End of transition time (sec), only neutral stability
trans_period <- trans_end - trans_begin
n <- 36 # number of sectors
sec_int <- 360/n # sector width in degrees
wind_var_1 <- 1 # interval of 1 m/s bins for lower wind speeds
max_speed <- 100 # set max wind speed
wind_int <- c(seq(0, 10, wind_var_1), c(11, max_speed)) # Create intervals for wind speeds
wind_class <- data.frame(wind_val = c(1:(length(wind_int) - 1))) # assigned values for each wind speed bin

# functions ---------------------------------------------------------------

#' Divide wind directions into 36 sectors centered on the source location and assign
#' a sector to each receptor location
assign_sector <- function(div_type, div_quantity) {
  data.frame(DIR = bearing(emis_source[source_id, c("lon", "lat")], 
                           div_type[1:div_quantity, c("lon", "lat")], f = 0)) %>% 
    mutate(DIR = 180 + DIR, # Add 180 degrees to direction that wind blows into sector 
           sec_val = findInterval(DIR, seq(0, 359.99999, sec_int), left.open = TRUE, rightmost.closed = TRUE), # assigns a value for each sector (e.g 1-36 for 3 wind direction bins)
           X = distm(    
             div_type[1:div_quantity, c("lon", "lat")],
             emis_source[source_id, c("lon", "lat")]
           )
    )
}


#' Compute transport time, vertical dispersion, and then estimate concentrations
#' with the gauss_plume function for emission sources at the ground level
estimate_concentration <- function(sector_type) {
  left_join(sector_type, joint_freq, by = "sec_val") %>%
    mutate(t = X / mean_SPD, # transport time
           neu_weight = case_when(
             t <= trans_begin ~ 0,
             t > trans_begin & t < trans_end ~ (t - trans_begin) / trans_period,
             t >= trans_end ~ 1
           )) %>% 
    mutate(stab_weight = 1 - neu_weight) %>% 
    mutate(s_z = case_when( 
      stab_class == "A" ~ exp(4.679 + (-1.7172 * (log(X))) + (0.2770 * (log(X))^2)),
      stab_class == "B" ~ exp(-1.999 + (0.8752 * (log(X))) + (0.0136 * (log(X))^2)),
      stab_class == "C" ~ exp(-2.341 + (0.9477 * (log(X))) + (-0.0020 * (log(X))^2)),
      stab_class == "D" ~ exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2)),
      stab_class == "E" ~ stab_weight * exp(-3.783 + (1.3010 * (log(X))) + (-0.0450 * (log(X))^2))
      + neu_weight * exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2)),
      stab_class == "F" ~ stab_weight * exp(-4.490 + (1.4024 * (log(X))) + (-0.0540 * (log(X))^2)) +
        +neu_weight * exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2))
    )) %>% 
    mutate(s_z = ifelse(s_z > sigmaz_thresh, sigmaz_thresh, s_z)) %>%
    gauss_plume() 
}
