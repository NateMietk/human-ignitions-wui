# This script is the first step in the WUI project. 
# Here we import, project, intersect, organize data layers
# Key layers are the Short ignitions, Radeloff WUI product, MTBS data

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(raster)
library(rgdal)
library(sf)
library(lubridate)
library(ncdf4)
library(doParallel)
library(foreach) 

# Helper functions --------------------------------------------------------
classify_wui <-  function(x) {
  # break out fires into small, med, large
  # input: 
  #   - x: vector of fire sizes
  # output: 
  #   - y: vector (same length) of classified fire sizes ----- Km2
  as.factor(ifelse(x == "Low_Dens_Intermix", "WUI",
                   ifelse(x == "Low_Dens_Interface", "WUI",
                          ifelse(x == "Med_Dens_Intermix", "WUI",
                                 ifelse(x == "Med_Dens_Interface", "WUI",
                                        ifelse(x == "High_Dens_Interface", "WUI",
                                               ifelse(x == "High_Dens_Intermix", "WUI",
                                                      ifelse(x == "Very_Low_Dens_Veg", "VLD", 
                                                             ifelse(x == "Uninhabited_Veg", "Wildlands",
                                                                    ifelse(x == "Med_Dens_NoVeg", "Urban",
                                                                           ifelse(x == "Low_Dens_NoVeg", "Urban",
                                                                                  ifelse(x == "High_Dens_NoVeg", "Urban",
                                                                                         "Other"))))))))))))
}

netcdf_import <- function(file) {
  file_split <- file %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  year <- substr(file_split[2], start = 1, stop = 4)
  
  start_date <- as.Date(paste(year, "01", "01", sep = "-"))
  end_date <- as.Date(paste(year, "12", "31", sep = "-"))
  date_seq <- seq(start_date, end_date, by = "1 day") %m+% years(1)
  
  nc <- nc_open(tmp_dl[1])
  nc_att <- attributes(nc$var)$names
  ncvar <- ncvar_get(nc, nc_att)
  rm(nc)
  proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  rbrck <- brick(ncvar, crs= proj)
  rm(ncvar)
  extent(rbrck) <- c(-124.793, -67.043, 25.04186, 49.41686)
  names(rbrck) <- paste(var, unique(date_seq),
                        sep = "-")
  return(rbrck)
}      

step1 <- function(x, mask, fun.a) {
  file_split <- x %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  year <- substr(file_split[2], start = 1, stop = 4)
  
  # if (as.numeric(year) < 1992) {
  #   return("Year outside range of consideration")
  # }
  # 
  # if (as.numeric(year) > 2015) {
  #   return("Year outside range of consideration")
  # }
  
  start_date <- as.Date(paste(year, "01", "01", sep = "-"))
  end_date <- as.Date(paste(year, "12", "31", sep = "-"))
  date_seq <- seq(start_date, end_date, by = "1 day") %m+% years(1)
  monthly_seq <- seq(start_date, end_date, by = "1 month")
  
  proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  rbrck <- brick(x, crs= proj)
  rbrck <- stackApply(rbrck, month(date_seq), fun = fun.a)
  rbrck <- flip(t(rbrck), direction = "x")
  rbrck <- mask(rbrck, mask)
  
  names(rbrck) <- paste(var, unique(year(monthly_seq)), 
                        ifelse(nchar(unique(month(monthly_seq))) == 1, 
                               paste0("0", unique(month(monthly_seq))), 
                               unique(month(monthly_seq))),
                        unique(month(monthly_seq, label = TRUE)),
                        sep = "_")
  
  return(rbrck)
}  

step2 <- function(x, start, end, var) {
  
  start_date <- as.Date(paste(start, "01", "01", sep = "-"))
  end_date <- as.Date(paste(end, "12", "31", sep = "-"))
  monthly_seq <- month(seq(start_date, end_date, by = "1 month"))
  
  normals <- stackApply(x, indices = monthly_seq, fun = mean)
  names(normals) <- paste(var, unique(month(monthly_seq, label = TRUE)),
                          sep = "_")
  
  dir.create(paste0(dir, "Short_Update/", dir_proc,  var,  "/"), showWarnings = FALSE)
  out <- paste0(dir, "Short_Update/", dir_proc,  var,  "/")
  writeRaster(normals, filename = paste0(out,names(normals)),
              format = "GTiff", bylayer=TRUE, overwrite = TRUE)
}

# Set directories and projections -----------------------------------------

dir <- ifelse(Sys.getenv("LOGNAME") == "NateM", "/Users/NateM/Dropbox/Professional/RScripts/",
              ifelse(Sys.getenv("LOGNAME") == "nami1114", "/Users/nami1114/Dropbox/Professional/RScripts/",
                     ifelse(Sys.getenv("LOGNAME") == "rana7082", "/Users/rana7082/Dropbox/", "C:/Users/rnagy/Dropbox/")))
dir_proc <- "data/processed/"

# To be used in the parallelized sections of the code
UseCores <- detectCores() -1

#EPSG:102003 USA_Contiguous_Albers_Equal_Area_Conic
proj_ea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#EPSG:102005 USA_Contiguous_Equidistant_Conic
proj_ed <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#WGS 84 the gridmet projection
proj_ll <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Import Shapefiles from Geodatabase -------------------------------------------------------

#Import the USA States layer
usa_shp <- st_read(dsn = paste0(dir, "Short_Update/data/bounds/state"),
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  subset(., NAME != "Alaska" &
           NAME != "Hawaii" &
           NAME != "Puerto Rico") %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         StArea_km2 = area_m2/1000000,
         group = 1) %>%
  st_simplify(., preserveTopology = TRUE) 
plot(usa_shp[5])

# Dissolve to the USA Boundary
conus <- usa_shp %>%
  group_by(group) %>%
  st_union()
plot(conus)

# Import the Level 3 Ecoregions
eco = paste0(dir, "Short_Update/data/bounds/us_eco_l3")
ecoreg <- st_read(dsn = eco, layer = "us_eco_l3", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
  mutate(area_m2 = as.numeric(st_area(geometry)),
         EcoArea_km2 = area_m2/1000000)
plot(ecoreg[2])

# Intersects the region 
state_eco <- st_intersection(usa_shp, ecoreg) %>%
  dplyr::select(STUSPS, NAME, StArea_km2, US_L3CODE, US_L3NAME, EcoArea_km2, NA_L2NAME, NA_L1CODE, NA_L1NAME, geometry)
plot(state_eco[2])


# Read the FPA database class
shrt_fire <- st_read(dsn = paste0(dir, "Short_Update/data/fire/fpa-fod/Data/FPA_FOD_20170508.gdb"),
                     layer = "Fires", quiet= TRUE) %>%
  st_transform(., proj_ea) %>%
  filter(STATE != "AK" & STATE != "PR" & STATE != "HI" & FIRE_SIZE >= 0.01) %>%
  dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME,
                FIRE_YEAR, DISCOVERY_DATE, DISCOVERY_DOY, STAT_CAUSE_DESCR, FIRE_SIZE, STATE) %>%
  mutate(IGNITION = ifelse(STAT_CAUSE_DESCR == "Lightning", "Lightning", "Human"),
         FIRE_SIZE_m2 = FIRE_SIZE*4046.86,
         FIRE_SIZE_km2 = FIRE_SIZE_m2/1000000,
         FIRE_SIZE_ha = FIRE_SIZE_m2*10000,
         DISCOVERY_DAY = day(DISCOVERY_DATE),
         DISCOVERY_MONTH = month(DISCOVERY_DATE),
         DISCOVERY_YEAR = FIRE_YEAR)

# Produce the monthly normals --> Wind -------------------------------------------------------------
# This task only needs to be run 1 time 
# After this is ran then you will have produced the monthly normals from 1992-2015
cl <- makeCluster(UseCores)

# Step 1
wind_dl <- list.files(paste0(dir, "Short_Update/data/climate/windsp"), pattern = "nc", full.names = TRUE)

mask <- st_transform(usa_shp, proj_ll)

# Create monthly means to raster list

# Not parallelized
#windsp <- lapply(wind_dl, step1, mask = as(mask, "Spatial"), fun.a = mean)

# Parallelized
windsp <- foreach(i = 1:length(wind_dl)) %dopar% {
  step1(wind_dl[i], as(mask, "Spatial"), mean)}
stopCluster(cl)

# Stack to master brick
windsp <- do.call(stack,windsp)

# Step 2
# Calculate normals and write out to GeoTif
# The variable windsp will be a usable raster stack
windsp <- step2(windsp, "1979", "1981", "tmp")

# Produce the monthly normals -->  Fuel Moisture -------------------------------------------------------------
# This task only needs to be run 1 time 
# After this is ran then you will have produced the monthly normals from 1992-2015
cl <- makeCluster(UseCores)

# Step 1
fm_dl <- list.files(paste0(dir, "Short_Update/data/veg/fm100"), pattern = "nc", full.names = TRUE)

# Create monthly means to raster list

# Not parallelized
#fm <- lapply(fm_dl, step1, mask = as(mask, "Spatial"), fun.a = mean)

# Parallelized
fm <- foreach(i = 1:length(fm_dl)) %dopar% {
  step1(fm_dl[i], as(mask, "Spatial"), mean)}
stopCluster(cl)

# Stack to master brick
fm <- do.call(stack,fm)

# Step 2
# Calculate normals and write out to GeoTif
# The variable fm will be a usable raster stack
fm <- step2(fm, "1979", "1981", "tmp")

# Extract normals LOOPS NOT WORKING------------------------------------------------

extract_function <- function(y, x){
  file_split <- y %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  
  rstb <- stack()
  for(i in 1:NROW(y)){
    tempraster <- raster(y[i])
    rstb <- stack(rstb,tempraster)
  }
  
  # shrt_wind <- list()
  # for(i in 1:nrow(x)){
  #   if(x$DISCOVERY_MONTH > 0) {
  #     raster::extract(rstb, as(x[i], "Spatial"), sp = TRUE)
  #   } 
  extracList <- vector("list", length(x)) 
  cnt <- 0 
  for(i in 1:NROW(x)){
    cnt <- cnt + 1 
    if(x$DISCOVERY_MONTH > 1) {
      extracList[[cnt]] <- raster::extract(rstb, as(x[i], "Spatial"), sp = TRUE)
    } 
    else {
      stop("Didn't work sucka")
    }
  }
}

extract_function <- function(y, x){
  file_split <- y %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  
  rstb <- stack()
  for(i in 1:NROW(y)){
    tempraster <- raster(y[i])
    rstb <- stack(rstb,tempraster)
  }
  
  extracList <- vector("list", length(x)) 
  cnt <- 0 
  for(i in 1:NROW(x)){
    cnt <- cnt + 1 
    if(x$DISCOVERY_MONTH > 1) {
      extracList[[cnt]] <- raster::extract(rstb, as(x[i], "Spatial"), sp = TRUE)
    } 
    else {
      stop("Didn't work sucka")
    }
  }
  return(extracList)
}

# Extract WIND normals to short ------------------------------------------------

wind_dl <- list.files(paste0(dir, "Short_Update/data/processed/vs/normals"), pattern = "tif", full.names = TRUE)

wind <- lapply(wind_dl, raster) 

shrt_jan <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "1") %>%
  st_transform(., proj_ll)
shrt_jan <- as(shrt_jan, "Spatial")
wind_jan <- windter::extract(wind[[5]],
                             shrt_jan, sp = TRUE) 
wind_jan <- st_as_sf(wind_jan) %>%
  mutate(Wind = vs_Jan) %>%
  select(-starts_with("vs_"))


shrt_feb <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "2") %>%
  st_transform(., proj_ll)
shrt_feb <- as(shrt_feb, "Spatial")
wind_feb <- windter::extract(wind[[4]],
                             shrt_feb, sp = TRUE) 
wind_feb <- st_as_sf(wind_feb) %>%
  mutate(Wind = vs_Feb) %>%
  select(-starts_with("vs_"))

shrt_mar <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "3") %>%
  st_transform(., proj_ll)
shrt_mar <- as(shrt_mar, "Spatial")
wind_mar <- windter::extract(wind[[8]],
                             shrt_mar, sp = TRUE) 
wind_mar <- st_as_sf(wind_mar) %>%
  mutate(Wind = vs_Mar) %>%
  select(-starts_with("vs_"))

shrt_apr <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "4") %>%
  st_transform(., proj_ll)
shrt_apr <- as(shrt_apr, "Spatial")
wind_apr <- windter::extract(wind[[1]],
                             shrt_apr, sp = TRUE) 
wind_apr <- st_as_sf(wind_apr) %>%
  mutate(Wind = vs_Apr) %>%
  select(-starts_with("vs_"))

shrt_may <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "5") %>%
  st_transform(., proj_ll)
shrt_may <- as(shrt_may, "Spatial")
wind_may <- windter::extract(wind[[9]],
                             shrt_may, sp = TRUE) 
wind_may <- st_as_sf(wind_may) %>%
  mutate(Wind = fm100_May) %>%
  select(-starts_with("fm100_"))

shrt_jun <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "6") %>%
  st_transform(., proj_ll)
shrt_jun <- as(shrt_jun, "Spatial")
wind_jun <- windter::extract(wind[[7]],
                             shrt_jun, sp = TRUE) 
wind_jun <- st_as_sf(wind_jun) %>%
  mutate(Wind = vs_Jun) %>%
  select(-starts_with("vs_"))

shrt_jul <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "7") %>%
  st_transform(., proj_ll)
shrt_jul <- as(shrt_jul, "Spatial")
wind_jul <- windter::extract(wind[[6]],
                             shrt_jul, sp = TRUE) 
wind_jul <- st_as_sf(wind_jul) %>%
  mutate(Wind = vs_Jul) %>%
  select(-starts_with("vs_"))

shrt_aug <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "8") %>%
  st_transform(., proj_ll)
shrt_aug <- as(shrt_aug, "Spatial")
wind_aug <- windter::extract(wind[[2]],
                             shrt_aug, sp = TRUE) 
wind_aug <- st_as_sf(wind_aug) %>%
  mutate(Wind = vs_Aug) %>%
  select(-starts_with("vs_"))

shrt_sep <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "9") %>%
  st_transform(., proj_ll)
shrt_sep <- as(shrt_sep, "Spatial")
wind_sep <- windter::extract(wind[[12]],
                             shrt_sep, sp = TRUE) 
wind_sep <- st_as_sf(wind_sep) %>%
  mutate(Wind = vs_Sep) %>%
  select(-starts_with("vs_"))

shrt_oct <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "10") %>%
  st_transform(., proj_ll)
shrt_oct <- as(shrt_oct, "Spatial")
wind_oct <- windter::extract(wind[[11]],
                             shrt_oct, sp = TRUE) 
wind_oct <- st_as_sf(wind_oct) %>%
  mutate(Wind = vs_Oct) %>%
  select(-starts_with("vs_"))

shrt_nov <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "11") %>%
  st_transform(., proj_ll)
shrt_nov <- as(shrt_nov, "Spatial")
wind_nov <- windter::extract(wind[[10]],
                             shrt_nov, sp = TRUE) 
wind_nov <- st_as_sf(wind_nov) %>%
  mutate(Wind = vs_Nov) %>%
  select(-starts_with("vs_"))

shrt_dec <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "12") %>%
  st_transform(., proj_ll)
shrt_dec <- as(shrt_dec, "Spatial")
wind_dec <- windter::extract(wind[[3]],
                             shrt_dec, sp = TRUE) 
wind_dec <- st_as_sf(wind_dec) %>%
  mutate(Wind = vs_Dec) %>%
  select(-starts_with("vs_"))

shrt_wind <- wind_jan %>%
  bind_rows(., wind_feb) %>%
  bind_rows(., wind_mar) %>%
  bind_rows(., wind_apr) %>%
  bind_rows(., wind_may) %>%
  bind_rows(., wind_jun) %>%
  bind_rows(., wind_jul) %>%
  bind_rows(., wind_aug) %>%
  bind_rows(., wind_sep) %>%
  bind_rows(., wind_oct) %>%
  bind_rows(., wind_nov) %>%
  bind_rows(., wind_dec)

# Extract FUEL MOISTURE normals to short ------------------------------------------------

fm_dl <- list.files(paste0(dir, "Short_Update/data/processed/fm100/normals"), pattern = "tif", full.names = TRUE)

fm <- lapply(fm_dl, raster) 

shrt_jan <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "1") %>%
  st_transform(., proj_ll)
shrt_jan <- as(shrt_jan, "Spatial")
fm_jan <- raster::extract(fm[[5]],
                          shrt_jan, sp = TRUE) 
fm_jan <- st_as_sf(fm_jan) %>%
  mutate(fm = fm100_Jan) %>%
  select(-starts_with("fm100"))


shrt_feb <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "2") %>%
  st_transform(., proj_ll)
shrt_feb <- as(shrt_feb, "Spatial")
fm_feb <- raster::extract(fm[[4]],
                          shrt_feb, sp = TRUE) 
fm_feb <- st_as_sf(fm_feb) %>%
  mutate(fm = fm100_Feb) %>%
  select(-starts_with("fm100"))

shrt_mar <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "3") %>%
  st_transform(., proj_ll)
shrt_mar <- as(shrt_mar, "Spatial")
fm_mar <- raster::extract(fm[[8]],
                          shrt_mar, sp = TRUE) 
fm_mar <- st_as_sf(fm_mar) %>%
  mutate(fm = fm100_Mar) %>%
  select(-starts_with("fm100"))

shrt_apr <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "4") %>%
  st_transform(., proj_ll)
shrt_apr <- as(shrt_apr, "Spatial")
fm_apr <- raster::extract(fm[[1]],
                          shrt_apr, sp = TRUE) 
fm_apr <- st_as_sf(fm_apr) %>%
  mutate(fm = fm100_Apr) %>%
  select(-starts_with("fm100_"))

shrt_may <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "5") %>%
  st_transform(., proj_ll)
shrt_may <- as(shrt_may, "Spatial")
fm_may <- raster::extract(fm[[9]],
                          shrt_may, sp = TRUE) 
fm_may <- st_as_sf(fm_may) %>%
  mutate(fm = fm100_May) %>%
  select(-starts_with("fm100_"))

shrt_jun <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "6") %>%
  st_transform(., proj_ll)
shrt_jun <- as(shrt_jun, "Spatial")
fm_jun <- raster::extract(fm[[7]],
                          shrt_jun, sp = TRUE) 
fm_jun <- st_as_sf(fm_jun) %>%
  mutate(fm = fm100_Jun) %>%
  select(-starts_with("fm100_"))

shrt_jul <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "7") %>%
  st_transform(., proj_ll)
shrt_jul <- as(shrt_jul, "Spatial")
fm_jul <- raster::extract(fm[[6]],
                          shrt_jul, sp = TRUE) 
fm_jul <- st_as_sf(fm_jul) %>%
  mutate(fm = fm100_Jul) %>%
  select(-starts_with("fm100_"))

shrt_aug <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "8") %>%
  st_transform(., proj_ll)
shrt_aug <- as(shrt_aug, "Spatial")
fm_aug <- raster::extract(fm[[2]],
                          shrt_aug, sp = TRUE) 
fm_aug <- st_as_sf(fm_aug) %>%
  mutate(fm = fm100_Aug) %>%
  select(-starts_with("fm100_"))

shrt_sep <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "9") %>%
  st_transform(., proj_ll)
shrt_sep <- as(shrt_sep, "Spatial")
fm_sep <- raster::extract(fm[[12]],
                          shrt_sep, sp = TRUE) 
fm_sep <- st_as_sf(fm_sep) %>%
  mutate(fm = fm100_Sep) %>%
  select(-starts_with("fm100_"))

shrt_oct <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "10") %>%
  st_transform(., proj_ll)
shrt_oct <- as(shrt_oct, "Spatial")
fm_oct <- raster::extract(fm[[11]],
                          shrt_oct, sp = TRUE) 
fm_oct <- st_as_sf(fm_oct) %>%
  mutate(fm = fm100_Oct) %>%
  select(-starts_with("fm100_"))

shrt_nov <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "11") %>%
  st_transform(., proj_ll)
shrt_nov <- as(shrt_nov, "Spatial")
fm_nov <- raster::extract(fm[[10]],
                          shrt_nov, sp = TRUE) 
fm_nov <- st_as_sf(fm_nov) %>%
  mutate(fm = fm100_Nov) %>%
  select(-starts_with("fm100_"))

shrt_dec <- shrt_fire %>%
  subset(DISCOVERY_MONTH == "12") %>%
  st_transform(., proj_ll)
shrt_dec <- as(shrt_dec, "Spatial")
fm_dec <- raster::extract(fm[[3]],
                          shrt_dec, sp = TRUE) 
fm_dec <- st_as_sf(fm_dec) %>%
  mutate(fm = fm100_Dec) %>%
  select(-starts_with("fm100_"))

shrt_fm <- fm_jan %>%
  bind_rows(., fm_feb) %>%
  bind_rows(., fm_mar) %>%
  bind_rows(., fm_apr) %>%
  bind_rows(., fm_may) %>%
  bind_rows(., fm_jun) %>%
  bind_rows(., fm_jul) %>%
  bind_rows(., fm_aug) %>%
  bind_rows(., fm_sep) %>%
  bind_rows(., fm_oct) %>%
  bind_rows(., fm_nov) %>%
  bind_rows(., fm_dec) 

shrt_fm_df <- as.data.frame(shrt_fm) %>%
  select("FPA_ID", "fm")

shrt_wind_fm <- shrt_wind %>%
  left_join.sf(., shrt_fm_df, by = "FPA_ID")

# Import biophysical setting ---------------------------------------------
bps <- raster(paste0(dir, "Short_Update/data/veg/US_130_BPS/grid/us_130bps"))
bps.ref <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
shrt_bps <- shrt_fire %>%
  st_transform(., bps.ref)
shrt_bps <- raster::extract(bps, as(shrt_bps, "Spatial"), sp = TRUE)
shrt_bps <- st_transform(shrt_bps, proj_ea)

# Import biomass data ----------------------------------------------------
bio <- raster(paste0(dir, "Short_Update/data/veg/WoodsHole_Biomass_240m/NBCD_countrywide_biomass_mosaic.tif"))
shrt_veg <- raster::extract(bio, as(shrt_bps, "Spatial"), sp = TRUE)

# Subset the FPA data to large fires (90th%tile) ---------------------------------------
cl <- makeCluster(UseCores)
lrg_shrt_fire <- foreach(i = 1:NROW(shrt_fire)) %dopar% {
  st_intersection(shrt_fire[i], ecoreg)} das
stopCluster(cl)

tt3<-unique(lrg_shrt_fire$L3CODE)
output=NULL
for (i in tt3) {
  subby<-shrt_fire[shrt_fire$L3CODE==i,]
  ninety<-subset(subby, FIRE_SIZE_ha >= quantile(FIRE_SIZE_ha, 0.90))
  output<-rbind(output,data.frame(ninety[,]))
}
