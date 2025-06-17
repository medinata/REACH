current_dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
plume_dir <- paste0(dirname(sub_dir), '/dispersion/')
model_dir <- paste0(current_dir, '/scripts/model/')

poll_var <- "SO2"

source_type <- "elevated"

source(paste0(plume_dir, poll_var,'.R'))

source(paste0(model_dir,'model_script.R'))
