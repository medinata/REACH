# Description -------------------------------------------------------------
#' Input data for Gaussian-plume dispersion calculations
#' Adjust variables and assumptions if needed

# Variable and function definition ----------------------------------------
#' eff_height: plume rise + stack height (m)
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


# References --------------------------------------------------------------
#' Plume rise calculations:
#' Seinfeld, J. H., & Pandis, S. N. (2006). Atmospheric chemistry and physics: 
#' From air pollution to climate change (2nd ed., p. 868). Wiley-Interscience.



# input parameters ---------------------------------------------------------------
if(source_type == 'ground') {
  eff_height = 10
}


sigmaz_thresh <- 5000 # Threshold for vertical stability 
trans_begin <- 12 * 3600 # Start of transition time (sec) from stable to neutral stability
trans_end <- 36 * 3600 # End of transition time (sec), only neutral stability
trans_period <- trans_end - trans_begin
n <- 36 # number of sectors
sec_int <- 10 # sector width in degrees
wind_var_1 <- 1 # interval of 1 m/s for lower wind speeds
max_speed <- 100 # set max wind speed
wind_int <- c(seq(0, 10, wind_var_1), c(11, max_speed)) # Create intervals for wind speeds
wind_class <- data.frame(wind_val = c(1:(length(wind_int) - 1))) # assigned values for each wind speed bin
g <- 9.807 # acceleration due to gravity, for plume rise calculations 



# Use potential gradient to compute plume rise 
pot_gradient <- data.frame(stab_class = c("A","B","C","D","E","F"),
                           pot_grad = c(-1.0,-0.8,-0.6,0,1.5,2.6)/100) 


# functions ---------------------------------------------------------------
#' Divide wind directions into 36 sectors centered on the source location and assign
#' a sector to each receptor location
assign_sector <- function(div_type, div_quantity) {
  data.frame(DIR = bearing(emis_source[source_id, c("lon", "lat")], 
                           div_type[1:div_quantity, c("lon", "lat")], f = 0)) %>% 
    mutate(DIR = 180 + DIR, # Add 180 degrees to direction that wind blows into sector 
           sec_val = findInterval(DIR, seq(0, 359.99999, sec_int), left.open = TRUE, rightmost.closed = TRUE),
           X = distm(
             div_type[1:div_quantity, c("lon", "lat")],
             emis_source[source_id, c("lon", "lat")]
           )
    )
}



# Briggs form of plume rise equation
rise_eq <- function(E, a, b, X, u) {
  E * (X^b) /(u ^a)
}


# Calculate plume rise  ---------------------------------------------------
apply_rise <- function(met_info) {
  mutate(met_info,
         plume_rise = case_when(
           stab_class %in% c("A", "B", "C", "D") & flux < 55 & (X < (49 * flux^(5 / 8)))
           ~ rise_eq(1.6 * flux^(1 / 3), 1, 2 / 3, X, mean_SPD),
           stab_class %in% c("A", "B", "C", "D") & flux < 55 & (X >= flux^(5 / 8))
           ~ rise_eq(21.4 * flux^(3 / 4), 1, 0, X, mean_SPD),
           stab_class %in% c("A", "B", "C", "D") & flux >= 55 & (X < (119 * (flux^(2 / 5))))
           ~ rise_eq(1.6 * flux^(1 / 3), 1, 2 / 3, X, mean_SPD),
           stab_class %in% c("A", "B", "C", "D") & flux >= 55 & (X >= (119 * (flux^(2 / 5))))
           ~ rise_eq(38.7 * flux^(3 / 5), 1, 0, X, mean_SPD)
         ),
         stab_1 = case_when(stab_class %in% c("E", "F")
                            ~ rise_eq(2.4 * ((flux / s_2)^(1 / 3)), 1 / 3, 0, X, mean_SPD)),
         stab_2 = case_when(stab_class %in% c("E", "F")
                            ~ rise_eq(5 * flux^(1 / 4) * s_2^(-3 / 8), 0, 0, X, mean_SPD)),
         stab_3 = case_when(stab_class %in% c("E", "F")
                            ~ rise_eq(1.6 * flux^(1 / 3), 1, 2 / 3, X, mean_SPD))
  )
}

applyto_rec <- function(rec_data) {
  mutate(rec_data,
         plume_rise = ifelse(stab_class %in% c("E","F"), pmin(stab_1,stab_2, stab_3), plume_rise),
         plume_rise = ifelse(mean_temp > stack_info$stktemp, 0, plume_rise ), #neglect momentum based plume rise 
         s_z = ifelse(s_z > sigmaz_thresh, sigmaz_thresh, s_z) # Cap vertical dispersion to prevent it from reaching unrealistic values
  )
}



# Gaussian-plume ----------------------------------------------------------
#' Compute transport time, vertical dispersion, and then estimate concentrations
#' with the gauss_plume function for emission sources at the ground level
estimate_norise <- function(sector_type) {
  left_join(sector_type, joint_freq, by = "sec_val") %>%
    mutate(t = X / mean_SPD,
           neu_weight = case_when(
             t <= trans_begin ~ 0,
             t > trans_begin & t < trans_end ~ (t - trans_begin) / trans_period,
             t >= trans_end ~ 1
           ),
           stab_weight = 1 - neu_weight,
           s_z = case_when(
             stab_class == "A" ~ exp(4.679 + (-1.7172 * (log(X))) + (0.2770 * (log(X))^2)),
             stab_class == "B" ~ exp(-1.999 + (0.8752 * (log(X))) + (0.0136 * (log(X))^2)),
             stab_class == "C" ~ exp(-2.341 + (0.9477 * (log(X))) + (-0.0020 * (log(X))^2)),
             stab_class == "D" ~ exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2)),
             stab_class == "E" ~ stab_weight * exp(-3.783 + (1.3010 * (log(X))) + (-0.0450 * (log(X))^2))
             + neu_weight * exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2)),
             stab_class == "F" ~ stab_weight * exp(-4.490 + (1.4024 * (log(X))) + (-0.0540 * (log(X))^2)) +
               +neu_weight * exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2))
           ),
           s_z = ifelse(s_z > sigmaz_thresh, sigmaz_thresh, s_z)
    ) %>%
    gauss_plume(.)
}

#' Compute transport time, vertical dispersion, and then estimate concentrations
#' with the gauss_plume function for emission sources an elevated height
estimate_elevated <- function(sector_type, height_name) {
  left_join(sector_type,joint_freq, by = "sec_val") %>% 
    mutate(t = X/mean_SPD,
           neu_weight = case_when(t <= trans_begin ~ 0,
                                  t > trans_begin & t < trans_end ~ (t-trans_begin)/trans_period,
                                  t >= trans_end ~ 1
           ),
           stab_weight = 1 - neu_weight,
           s_z = case_when(
             stab_class == "A" ~ exp(4.679 + (-1.7172 * (log(X))) + (0.2770 * (log(X))^2)),
             stab_class == "B" ~ exp(-1.999 + (0.8752 * (log(X))) + (0.0136 * (log(X))^2)),
             stab_class == "C" ~ exp(-2.341 + (0.9477 * (log(X))) + (-0.0020 * (log(X))^2)),
             stab_class == "D" ~ exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2)),
             stab_class == "E" ~ stab_weight * exp(-3.783 + (1.3010 * (log(X))) + (-0.0450 * (log(X))^2))
             + neu_weight * exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2)),
             stab_class == "F" ~ stab_weight * exp(-4.490 + (1.4024 * (log(X))) + (-0.0540 * (log(X))^2)) +
               +neu_weight * exp(-3.186 + (1.1737 * (log(X))) + (-0.0316 * (log(X))^2))
           )) %>%
    apply_rise(.) %>% 
    applyto_rec(.) %>% 
    mutate(., eff_height = height_name + plume_rise
    ) %>%
    gauss_plume(.)
}
