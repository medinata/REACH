# Description -------------------------------------------------------------
#' Version A REACH script for generating source-receptor matrices


# load libraries ------------------------------------------------------------
library(tidyverse)
library(geosphere)


# directory ---------------------------------------------------------------
open_dir <- function(name_ext) {
  paste0(getwd(),'/', name_ext, '/')
}

input_dir <- open_dir('inputs')
result_dir <- open_dir('results/matrices')
met_dir <- open_dir('inputs/met_data')
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
emis_dir <- open_dir('inputs/emissions')


# load input files --------------------------------------------------------
source(paste0(script_dir, '/input_info.R'))

source(paste0(script_dir, '/domain_inputs.R'))


# output files ------------------------------------------------------------
SR_poll <- list() # source-receptor matrix


# run code ----------------------------------------------------------------
for (source_id in 1:nrow(emis_source)) {
  # meteorology for source location
  source_met <- read_csv(paste0(met_dir, emis_source[source_id, Input$ID], ".csv"))
  
  # extract stack height information for emission sources with stacks
  if (source_type %in% c("egu", "nonegu")) {
    stack_info <- emis_source[source_id, ] %>%
      select(stkhgt:stkvel)
  }
  
  # number of hours in a year
  hours <- nrow(source_met)
  
  
  # Calculate angle between source and receptor (sector angle that wind blows into receptor)
  sector <- assign_sector(receptors, Input$n) %>%
    cbind(receptors[, Input$ID])
  colnames(sector)[4] <- Input$ID
  
  # Determine wind speed and direction interval 
  interval_assn <- mutate(source_met,
                          sec_val = findInterval(DIR, seq(10, 360, sec_int)),
                          wind_val = findInterval(SPD, wind_int)
  ) %>%
    left_join(wind_class)
  
  # Calculate joint frequency of stability class and wind speed interval for a given sector
  joint_freq <- interval_assn %>%
    group_by(sec_val, stab_class, wind_val) %>%
    dplyr::summarize(
      count = n(), mean_SPD = mean(SPD), mean_temp = mean(TEMP),
      t_SO2 = mean(t_SO2), t_chem = mean(t_chem), freq = count / hours,
      .groups = "drop"
    )
  
  # Compute parameters for plume rise
  if (source_type %in% c("egu", "nonegu")) {
    joint_freq <- left_join(joint_freq, pot_gradient, by = "stab_class") %>%
      mutate(stack_info,
             flux = (g * (stkdiam^2) * stkvel * (stktemp - mean_temp)) / (4 * stktemp),
             s_2 = case_when(stab_class %in% c("E", "F") ~ (g * pot_grad) / (mean_temp))
      ) %>%
      select(c(
        "sec_val", "stab_class", "mean_SPD", "mean_temp", "t_SO2", "t_chem", "freq",
        "pot_grad", "flux", "s_2"
      ))
  }
  
  
  poll_final <- data.frame(matrix(ncol = 2, nrow = Input$n))
  colnames(poll_final) <- c(Input$ID, "conc")
  
  # Dispersion outside source location --------------------------------------
  #' Calculate marginal concentrations for all locations except the source location
  for (receptor_id in 1:nrow(sector)) {
    if(source_type %in% c('ground','elevated')) {
      rec_met <- estimate_norise(sector[receptor_id, ]) %>%
        select(Input$ID, conc)
    }
    
    
    if (source_type %in% c("egu", "nonegu")) {
      rec_met <- estimate_elevated(sector[receptor_id, ], emis_source[source_id, "stkhgt"]) %>%
        select(Input$ID, conc)
    }

    
    poll_final[receptor_id, 1] <- sector[receptor_id, Input$ID]
    poll_final[receptor_id, 2] <- sum(rec_met$conc)
  }
  
  # Dispersion within source location ---------------------------------------
  #' Calculate the marginal concentration at the source location by averaging marginal concentrations
  #' of grids within the source location geography
  if (!source_type %in% c('egu')) {
    subs <- filter(sub_division, get(Input$ID) == emis_source[source_id, Input$ID])
    
    t_sub <- nrow(subs)
    
    
    sector_sub <- assign_sector(subs, t_sub)
    
    
    poll_final_sub <- subs[, c(Input$ID, "lon", "lat")] %>%
      mutate(PM = NA)
    
    
    for (sub_id in 1:nrow(sector_sub)) {
      if(source_type %in% c('ground','elevated')) {
        rec_met_sub <- estimate_norise(sector_sub[sub_id, ]) %>%
          select(conc)
      }
      
      
      if (source_type %in% c("egu", "nonegu")) {
        rec_met_sub <- estimate_elevated(sector_sub[sub_id, ], emis_source[source_id, "stkhgt"]) %>%
          select(conc)
      }
      
      poll_final_sub[sub_id, "conc"] <- sum(rec_met_sub$conc)
    }
    
    # Put the within-source concentration in the S-R matrix
    source_index <- which(receptors[, Input$ID] == emis_source[source_id, Input$ID])
    
    
    poll_final[source_index, "conc"] <- mean(poll_final_sub$conc)
    
  }
  
  # Add the concentration vector as one row in the source-receptor matrix
  SR_poll[[source_id]] <- poll_final$conc
  
  # Progress tracker for code
  print(source_id / nrow(emis_source) * 100)
}


# source-receptor matrix --------------------------------------------------
SR_poll_final <- data.frame(do.call(rbind, SR_poll))

write_csv(SR_poll_final, paste0(result_dir, "SR_", poll_var, "_", source_type, ".csv"))


