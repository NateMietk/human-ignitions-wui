
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

# Download the Level 2 Ecoregions
ecoregion_shp <- file.path(ecoregion_prefix, "NA_CEC_Eco_Level2.shp")
if (!file.exists(ecoregion_shp)) {
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l2.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  assert_that(file.exists(ecoregion_shp))
}

# Download the Level 1 Ecoregions
ecoregion_shp <- file.path(ecoregion_prefix, "NA_CEC_Eco_Level1.shp")
if (!file.exists(ecoregion_shp)) {
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  assert_that(file.exists(ecoregion_shp))
}

#Download the WUI shapefile
# Unzip does not work with files >4GB, will need to download on your own.

wui_gdb <- file.path(wui_prefix, "CONUS_WUI_cp12_d.gdb")
if (!file.exists(wui_gdb)) {
  #loc <- "http://silvis.forest.wisc.edu/sites/default/files/maps/wui/2010/gis/us_wui_2010.zip"
  loc <- 'http://silvis.forest.wisc.edu/GeoData/WUI_cp12/zip/fgdb/CONUS_WUI_cp12_fgdb.zip'
  dest <- paste0(wui_prefix, "/us_wui", ".zip")
  download.file(loc, dest)
  system(paste0("unzip ",
                dest,
                " -d ",
                wui_prefix))
  unlink(dest)
  assert_that(file.exists(wui_gdb))

  system(paste0("aws s3 sync ", raw_prefix, " ", s3_raw_prefix))
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
  system(paste0("aws s3 sync ",
                raw_prefix, " ",
                s3_raw_prefix))
}

#Download the MTBS fire polygons

mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2', 'dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0(mtbs_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = mtbs_prefix)
  unlink(dest)
  assert_that(file.exists(mtbs_shp))
  system(paste0("aws s3 sync ",
                raw_prefix, " ",
                s3_raw_prefix))
}

geomac_shp <- file.path(geomac_raw_dir, 'US_HIST_FIRE_PERIMTRS_DD83.gdb')
if (!file.exists(geomac_shp)) {
  loc <- "https://rmgsc.cr.usgs.gov/outgoing/GeoMAC/historic_fire_data/US_HIST_FIRE_PERIMTRS_DD83.gdb.zip"
  dest <- paste0(geomac_raw_dir, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = geomac_raw_dir)
  unlink(dest)
  assert_that(file.exists(geomac_shp))
  system(paste0("aws s3 sync ",
                raw_prefix, " ",
                s3_raw_prefix))
}





