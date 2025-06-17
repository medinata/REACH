# Description -------------------------------------------------------------
#' Script to organize ERA5 hourly met data for the REACH model. This script
#' uses the Southern Africa domain as an example. Replace the list of centoids
#' and ERA5 nc file for the domain of interest


# References --------------------------------------------------------------
#' ERA5 hourly data on single levels from 1959 to present -
#' https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview

#' SO2 and SO4 timescales -
#' from the CRDM model (Latimer, Douglas A., and Bruce Polkowsky. 1996. 
#' Particulate Matter Source-Receptor Relationships between All Point and 
#' Area Sources in the United States and PSD Class I Area Receptors. 
#' US Environmental Protection Agency, Office of Air Quality Planning and Standards.)
#' 
#' See REACH paper for information about the timescales

#' Pasquill-Gifford stability classifications - 
#' EPA Meteorological Monitoring Guidance for Regulatory Modeling Applications


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidync)
library(suncalc)

# Directories -------------------------------------------------------------
input_dir <- paste0(getwd(), '/files/')
export_dir <- paste0(input_dir, 'weather/')


# Inputs to change --------------------------------------------------------
#' Change to list of centroids for your domain
census_div <- read_csv(paste0(input_dir, 'southern_africa.csv')) 

#' Download nc file for your domain
met_file <- paste0(input_dir, "sa_met.nc") #' ERA5 reanalysis data extracted for the Southern Africa region
met_data <- tidync(met_file)
meta_info <- ncmeta::nc_atts(met_file)

time_info <- meta_info %>% 
             filter(name == 'units' & variable == 'time') %>% 
             pull(value) %>% 
             unlist()

#' Change value of 'origin_time' variable based on reference time in meta_info
origin_time <- "1900-01-01 00:00:00" #' hours are reported since this reference date, see meta_info variable and adjust
origin_units <-  unlist(strsplit(time_info, "\\s+"))[1]  #' units for the reference date
if (origin_units == 'hours') {
  convert_to_sec <- 3600 # hours to seconds
}

#' The newer version of the ERA5 nc files report the time in seconds. Check
#' meta_info for the 'valid_time' variable 
if (origin_units == 'seconds') {
  convert_to_sec <- 1 
}


# SO2 deposition timescale in seconds  ------------------------------------
t_SO2_D <- 1.7 * 86400 # 1.7 days to seconds


# function ---------------------------------------------------------------
#' Compute sunrise and sunset times to distinguish between day and night
#' times for stability class assignment
get_sunvar <- function(sun_var, date) {
  getSunlightTimes(date,
                   lat = lat_val, lon = lon_val,
                   keep = sun_var, tz = "UTC"
  ) %>% select(all_of(sun_var))
}



# process meterology ------------------------------------------------------
for (i in 1:nrow(census_div)) {
  #'  ERA5 data is reported on a lon, lat grid with 0.25 degree increments 
  #'  Obtain spatial coordinates for reanalysis that are closest to 
  #'  0.25th degree of local municipality centroids
  lon_val <- plyr::round_any(census_div$lon[i], 0.25)
  lat_val <- plyr::round_any(census_div$lat[i], 0.25)
  
  
  #' Subset reanalysis for lon and lat range to prevent loading all data
  lonrange <- c(lon_val - 0.1, lon_val + 0.1)
  latrange <- c(lat_val - 0.1, lat_val + 0.1)
  
  
  met_raw <- tidync(met_file) %>%
    hyper_filter(
      longitude = between(longitude, lonrange[1], lonrange[2]),
      latitude = between(latitude, latrange[1], latrange[2])
    ) %>% #' Obtain data within the specified range
    hyper_tibble() %>%
    filter(longitude == lon_val & latitude == lat_val) %>% #' Obtain data for the specified lat and lon for the local municipality
    mutate(date_time = as.POSIXct(origin_time, tz = "UTC") + time * convert_to_sec, #' Convert dates into YYYYMMDD format
           date = as.Date(format(date_time, "%Y-%m-%d"))
    )
  
  
  #' Compute sunrise and sunset times 
  sun_time <- data.frame(
    sunrise = get_sunvar("sunrise", met_raw$date),
    sunset = get_sunvar("sunset", met_raw$date)
  )
  
  met_process <- mutate(met_raw,
                        sunrise = sun_time$sunrise,
                        sunset = sun_time$sunset,
                        SPD = sqrt(u10^2 + v10^2), #' resultant  10 m wind speed based on vertical and horizontal wind vectors
                        DIR = 180 + atan2(v10, u10) * (180 / pi), #' wind direction in degrees
                        RAD = ssrd / 3600, #' solar radiation converted from J/m2 to W/m2,
                        VP_s = (exp(((1 / 273) - (1 / t2m)) * (2.453E6 / 461))) * 6.11, #' saturated vapor pressure
                        VP = (exp(((1 / 273) - (1 / d2m)) * (2.453E6 / 461))) * 6.11, #'vapor pressure
                        RH = (VP / VP_s) * 100, #' relative humidity, calculate for determining SO2 oxidation rates
                        SO2_OxRate = case_when( #' SO2 oxidation conversion rates in %/hr
                          RH < 40 ~ 0.5,
                          RH > 70 ~ 1.5,
                          RH >= 40 & RH <= 70 ~ ((RH - 40) / 30) + 0.5
                        ),
                        t_chem = (1 / (SO2_OxRate / 100)) * 3600, #' SO2 oxidation timescale in seconds
                        t_SO2 = ((1 / t_chem) + (1 / t_SO2_D))^-1 #' combined oxidation and deposition timescale in seconds 
  ) %>% 
    rename(SKC = tcc, TEMP = t2m)
  
  # Assign stability class based on Pasquill-Gifford classifications 
  met_class <- mutate(met_process,
                      day_night = ifelse(date_time < (sunset - 3600) &
                                           date_time > (sunrise + 3600),
                                         "day", "night"
                      ),
                      stab_class = case_when(
                        SKC == 1 ~ "D", # Assign neutral stability regardless of time of day for overcast conditions
                        day_night == "night" & SPD < 3 & SKC >= 0.5 & SKC < 1.0 ~ "E",
                        day_night == "night" & SPD >= 3 & SKC >= 0.5 & SKC < 1.0 ~ "D",
                        day_night == "night" & SPD < 3 & SKC < 0.5 ~ "F",
                        day_night == "night" & SPD >= 3 & SPD < 5 & SKC < 0.5 ~ "E",
                        day_night == "night" & SPD >= 5 ~ "D",
                        day_night == "day" & SPD < 3 & RAD >= 925 ~ "A",
                        day_night == "day" & SPD >= 3 & SPD < 5 & RAD >= 925 ~ "B",
                        day_night == "day" & SPD >= 5 & RAD >= 925 ~ "C",
                        day_night == "day" & SPD < 2 & RAD >= 675 & RAD < 925 ~ "A",
                        day_night == "day" & SPD >= 2 & SPD < 5 & RAD >= 675 & RAD < 925 ~ "B",
                        day_night == "day" & SPD >= 5 & SPD < 6 & RAD >= 675 & RAD < 925 ~ "C",
                        day_night == "day" & SPD >= 6 & RAD >= 675 & RAD < 925 ~ "D",
                        day_night == "day" & SPD < 2 & RAD >= 175 & RAD < 675 ~ "B",
                        day_night == "day" & SPD >= 2 & SPD < 5 & RAD >= 175 & RAD < 675 ~ "C",
                        day_night == "day" & SPD >= 5 & RAD >= 175 & RAD < 675 ~ "D",
                        day_night == "day" & RAD < 175 ~ "D"
                      )
  ) %>% 
    select(date_time, SPD, DIR, t_chem, t_SO2, TEMP, stab_class)
  
  # Save file for each other source location
  write_csv(met_class, paste0(export_dir, census_div$census_id[i], ".csv"))
  
  #' progress tracker
  if (i %% 100 == 0) {
    print(i / nrow(census_div) * 100)
  }
  
}



