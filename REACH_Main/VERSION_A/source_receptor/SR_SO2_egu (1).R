sub_dir <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
plume_dir <- paste0(dirname(sub_dir), '/dispersion/')
model_dir <- paste0(sub_dir, '/model/')

poll_var <- "SO2"

source_type <- "egu"

source(paste0(plume_dir, poll_var,'.R'))


source(paste0(model_dir,'model_script.R'))
