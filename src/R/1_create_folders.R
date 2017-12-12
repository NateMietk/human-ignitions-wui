# Libraries ---------------------------------------------------------------
x <- c("data.table", "tidyverse", "magrittr", "sf", "gridExtra", "rgdal", "raster", "rgeos", "data.table",
       "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel", "sp", "RColorBrewer", "ggmap")
lapply(x, library, character.only = TRUE, verbose = FALSE)

source("src/functions/helper_functions.R")
source("src/functions/make_grid.R")

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ecoregion_prefix <- file.path(raw_prefix, "us_eco_l3")
wui_prefix <- file.path(raw_prefix, "us_wui_2010")
fpa_prefix <- file.path(raw_prefix, "fpa-fod")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")

# Cleaned data output folders
bounds_crt <- file.path(prefix, "bounds")
conus_crt <- file.path(bounds_crt, "conus")
ecoreg_crt <- file.path(bounds_crt, "ecoregion")
anthro_out <- file.path(prefix, "anthro")
fire_crt <- file.path(prefix, "fire")

us_out <- file.path(conus_crt, "cb_2016_us_state_20m")
ecoregion_out <- file.path(ecoreg_crt, "us_eco_l3")
fpa_out <- file.path(fire_crt, "fpa-fod")
mtbs_out <- file.path(fire_crt, "mtbs_fod_perimeter_data")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ecoregion_prefix, wui_prefix, fpa_prefix, mtbs_prefix, 
                bounds_crt, conus_crt, ecoreg_crt, anthro_out, fire_crt, 
                us_out, ecoregion_out, fpa_out, mtbs_out)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))
