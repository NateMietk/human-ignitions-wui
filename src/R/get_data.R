
library(tidyverse)
library(assertthat)
library(rvest)
library(httr)
library(purrr)

raw_prefix <- file.path("data", "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ecoregion_prefix <- file.path(raw_prefix, "us_eco_l3")
wui_prefix <- file.path(raw_prefix, "us_wui_2010")
fpa_prefix <- file.path(raw_prefix, "fpa-fod")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(raw_prefix,
                us_prefix,
                ecoregion_prefix,
                wui_prefix,
                fpa_prefix,
                mtbs_prefix)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

#Download the USA States layer

us_shp <- file.path(us_prefix, "cb_2016_us_state_20m.shp")
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  dest <- paste0(us_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = us_prefix)
  unlink(dest)
  assert_that(file.exists(us_shp))
}

#Download the Level 3 Ecoregions

ecoregion_shp <- file.path(ecoregion_prefix, "us_eco_l3.shp")
if (!file.exists(ecoregion_shp)) {
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  assert_that(file.exists(ecoregion_shp))
}

#Download the WUI shapefile

wui_gdb <- file.path(wui_prefix, "us_wui_2010.gdb")
if (!file.exists(wui_gdb)) {
  loc <- "http://silvis.forest.wisc.edu/sites/default/files/maps/wui/2010/gis/us_wui_2010.zip"
  dest <- paste0(wui_prefix, ".zip")
  download.file(loc, dest)
  unlink(dest)
  assert_that(file.exists(wui_gdb))
}

# Download the FPA-FOD data

fpa_gdb <- file.path(fpa_prefix, "Data", "FPA_FOD_20170508.gdb")
if (!file.exists(fpa_gdb)) {
  pg <- read_html("https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.4/")
  fils <- html_nodes(pg, xpath=".//dd[@class='product']//li/a[contains(., 'zip') and contains(., 'GDB')]") 
  dest <- paste0(fpa_prefix, ".zip")
  walk2(html_attr(fils, 'href'),  html_text(fils), 
        ~GET(sprintf("https:%s", .x), write_disk(dest), progress()))
  unzip(dest, exdir = fpa_prefix)
  unlink(dest)
  assert_that(file.exists(fpa_gdb))
}

#Download the MTBS fire polygons

mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perims_1984-2015_DD_20170815.shp')
if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0(mtbs_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = mtbs_prefix)
  unlink(dest)
  assert_that(file.exists(fpa_gdb))
}
