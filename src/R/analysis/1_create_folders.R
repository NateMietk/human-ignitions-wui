# Libraries ---------------------------------------------------------------
x <- c("data.table", "tidyverse", "magrittr", "sf", "gridExtra", "rgdal", "raster", "rgeos", "data.table", 'lwgeom', 'nabor', 'velox', 'Hmisc', 'pbapply',
       "assertthat", "purrr", "httr", 'zoo', "rvest", "lubridate", "doParallel", "sp", "RColorBrewer", "ggmap", "ggthemes", 'snowfall', 'parallel')
lapply(x, library, character.only = TRUE, verbose = FALSE)

source("src/functions/helper_functions.R")
source("src/functions/make_grid.R")
source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" #USA_Contiguous_Equidistant_Conic
proj_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
proj_ztrax <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ncores <- 5

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ecoregion_prefix <- file.path(raw_prefix, "ecoregions")
wui_prefix <- file.path(raw_prefix, "us_wui")
fpa_prefix <- file.path(raw_prefix, "fpa-fod")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")
ztrax_prefix <- file.path(raw_prefix, 'ZTRAX_ZASMT_UTMAIN_SPATIAL_CLEAN')

# Cleaned data output folders
bounds_crt <- file.path(prefix, "bounds")
ecoreg_crt <- file.path(bounds_crt, "ecoregions")
swse_crt <- file.path(ecoreg_crt, "swse")
ecoregion_out <- file.path(ecoreg_crt, "us_eco_l3")

anthro_out <- file.path(prefix, "anthro")
wui_out <- file.path(anthro_out, "wui")
distance_out <- file.path(wui_out, "distance_from_urban")
ztrax_out <- file.path(anthro_out, 'ztrax')
zpoints_out <- file.path(ztrax_out, 'ztrax_points')
zpoints_res <- file.path(zpoints_out, 'ztrax_residential')
zpoints_abu <- file.path(zpoints_out, 'ztrax_all_built_up')

#all built up
dir_raw_abu_gpkg <- file.path(zpoints_abu, 'raw_all_built_up_gpkg')
dir_wui_abu_rds <- file.path(zpoints_abu, 'wui_all_built_up_rds')
dir_cleaned_wui_abu_rds <- file.path(zpoints_abu, 'cleaned_wui_all_built_up_rds')
dir_fpa_abu_rds <- file.path(zpoints_abu, 'fpa_all_built_up_rds')
dir_cleaned_fpa_abu_rds <- file.path(zpoints_abu, 'cleaned_fpa_all_built_up_rds')
dir_fpa_250m_abu_rds <- file.path(zpoints_abu, 'fpa_250m_all_built_up_rds')
dir_cleaned_fpa_250m_abu_rds <- file.path(zpoints_abu, 'cleaned_fpa_250m_all_built_up_rds')

#residential homes only
dir_raw_res_gpkg <- file.path(zpoints_res, 'raw_residential_gpkg')
dir_wui_res_rds <- file.path(zpoints_res, 'wui_residential_rds')
dir_cleaned_wui_res_rds <- file.path(zpoints_res, 'cleaned_wui_residential_rds')
dir_fpa_res_rds <- file.path(zpoints_res, 'fpa_residential_rds')
dir_cleaned_fpa_res_rds <- file.path(zpoints_res, 'cleaned_fpa_residential_rds')
dir_fpa_250m_res_rds <- file.path(zpoints_res, 'fpa_250m_residential_rds')
dir_cleaned_fpa_250m_res_rds <- file.path(zpoints_res, 'cleaned_fpa_250m_residential_rds')

bui_out <- file.path(ztrax_out, 'built_up_intensity')
bui_out <- file.path(bui_out, 'BUI')
bu_out <- file.path(ztrax_out, 'building_counts')
bu_out <- file.path(bu_out, 'building_counts_all')

fire_crt <- file.path(prefix, "fire")
nifc_crt <- file.path(fire_crt, "nifc")
fpa_out <- file.path(fire_crt, "fpa-fod")
mtbs_out <- file.path(fire_crt, "mtbs_fod_perimeter_data")
fire_pnt <- file.path(fpa_out, 'points')
fire_poly <- file.path(fpa_out, 'perimeters')

ics_out <- file.path(fire_crt, "ics_209")
ics_outtbls <- file.path(ics_out, "output_tbls")
ics_intbls <- file.path(ics_out, "input_tbls")
ics_famweb <- file.path(ics_intbls, "famweb")
ics_latlong <- file.path(ics_intbls, "latlong")
ics_spatial <- file.path(ics_out, "spatial")
rmarkdown_files <- file.path(prefix, 'rmarkdown_files')
fishnet_path <- file.path(bounds_crt, "fishnet")

# for pushing and pulling to s3 using the system function
s3_base <- 's3://earthlab-natem/human-ignitions-wui'
s3_distance <- 's3://earthlab-natem/human-ignitions-wui/anthro/wui/distance_from_urban'
s3_bounds_prefix <- 's3://earthlab-natem/human-ignitions-wui/bounds'
s3_anthro_prefix <- 's3://earthlab-natem/human-ignitions-wui/anthro'
s3_fire_prefix <- 's3://earthlab-natem/human-ignitions-wui/fire'
s3_raw_prefix <- 's3://earthlab-natem/human-ignitions-wui/raw'

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ecoregion_prefix, wui_prefix, fpa_prefix, mtbs_prefix, nifc_crt, ztrax_out, ztrax_prefix,
                bounds_crt, ecoreg_crt, anthro_out, fire_crt, ics_out, ics_outtbls, ics_intbls, wui_out, distance_out, bui_out, bu_out,
                swse_crt, ics_famweb, ics_latlong, ics_spatial, ecoregion_out, fpa_out, mtbs_out, fishnet_path, fire_pnt, fire_poly, 
                zpoints_out, zpoints_res, dir_raw_res_gpkg, dir_wui_res_rds, dir_fpa_res_rds, dir_cleaned_fpa_res_rds, dir_cleaned_wui_res_rds,
                dir_fpa_250m_res_rds, dir_cleaned_fpa_250m_res_rds,
                zpoints_abu, dir_raw_abu_gpkg, dir_wui_abu_rds, dir_cleaned_wui_abu_rds, dir_fpa_abu_rds, dir_cleaned_fpa_abu_rds, 
                dir_fpa_250m_abu_rds, dir_cleaned_fpa_250m_abu_rds, rmarkdown_files)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))
