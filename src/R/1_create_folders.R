# Libraries ---------------------------------------------------------------
x <- c("data.table", "tidyverse", "magrittr", "sf", "gridExtra", "rgdal", "raster", "rgeos", "data.table", "readtext",
       "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel", "sp", "RColorBrewer", "ggmap", "tabulizer", "ggthemes")
lapply(x, library, character.only = TRUE, verbose = FALSE)

source("src/functions/helper_functions.R")
source("src/functions/make_grid.R")
source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" #USA_Contiguous_Equidistant_Conic
ncores <- detectCores()

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ecoregion_prefix <- file.path(raw_prefix, "us_eco_l3")
wui_prefix <- file.path(raw_prefix, "us_wui_2010")
fpa_prefix <- file.path(raw_prefix, "fpa-fod")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")

# Cleaned data output folders
anthro_out <- file.path(prefix, "anthro")
fire_crt <- file.path(prefix, "fire")

us_out <- file.path(conus_crt, "cb_2016_us_state_20m")
ecoregion_out <- file.path(ecoreg_crt, "us_eco_l3")
fpa_out <- file.path(fire_crt, "fpa-fod")
mtbs_out <- file.path(fire_crt, "mtbs_fod_perimeter_data")
ics_out <- file.path(fire_crt, "ics_209")
ics_outtbls <- file.path(ics_out, "output_tbls")
ics_intbls <- file.path(ics_out, "input_tbls")
ics_famweb <- file.path(ics_intbls, "famweb")
ics_latlong <- file.path(ics_intbls, "latlong")

ics_spatial <- file.path(ics_out, "spatial")


# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ecoregion_prefix, wui_prefix, fpa_prefix, mtbs_prefix, nifc_crt,
                bounds_crt, conus_crt, ecoreg_crt, anthro_out, fire_crt, ics_out, ics_outtbls, ics_intbls,
                ics_famweb, ics_latlong, ics_spatial, us_out, ecoregion_out, fpa_out, mtbs_out)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))
