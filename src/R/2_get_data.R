
source("src/R/1_create_folders.R")

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

# Download the Level 3 Ecoregions
ecoregion_shp <- file.path(ecoregion_prefix, "us_eco_l3.shp")
if (!file.exists(ecoregion_shp)) {
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  assert_that(file.exists(ecoregion_shp))
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

wui_gdb <- file.path(wui_prefix, "us_wui_2010.gdb")
if (!file.exists(wui_gdb)) {
  loc <- "http://silvis.forest.wisc.edu/sites/default/files/maps/wui/2010/gis/us_wui_2010.zip"
  dest <- paste0(wui_prefix, "/us_wui_2010", ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = wui_prefix)
  unlink(wui_prefix)
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
