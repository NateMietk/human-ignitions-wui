anom_fun <- function(x, y) {
  x - y
}
mean_fun <- function(x, y) {
  (x + y)/2
}
celsius <- function(x) {
  x - 272.15
}

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

subset_ztrax <- function(i, gdbs, usa_shp, out_dir) {
  require(sf)
  require(tidyverse)
  
  outname <- basename(i) %>%
    gsub('.gdb', '.gpkg', .)
  
  if(!file.exists(file.path(out_dir, outname))) {
    
    layers <- tibble::data_frame(name = sf::st_layers(i)$name)
    
    state_ztrax <- apply(unique(layers), 1,
                         FUN = function(j) {
                           state_ztrax <- sf::st_read(i, layer = j) %>%
                             dplyr::filter(geom_wkt != 'POINT(0 0)') %>%
                             dplyr::filter(!(LU_stdcode %in% c('VL', 'AG', 'TR', 'MS'))) %>%
                             dplyr::filter(!(LU_desc %in% c('EI100', 'EI109', 'EI110', 'EI111', 'EI113',
                                                            'EI14', 'GV111', 'GV118', 'IH103', 'IH122', 
                                                            'IN104', 'RC102', 'RC103', 'PP102', 'RC106', 
                                                            'RC113', 'RC124', 'RC129', 'RC130', 'IH113', 'IH115',
                                                            'IN113'))) %>%
                             dplyr::mutate(built_class = ifelse(str_detect(LU_stdcode, 'RI|RR'), 'Residential', 'Non-Residential')) %>%
                             dplyr::select(-geom_wkt, -ImptPrclID, -LU_stdcode, -LU_desc, -LU_code)
                         })
    
    do.call(rbind, state_ztrax) %>%
      sf::st_transform(sf::st_crs(usa_shp)) %>%
      sf::st_write(., file.path(out_dir, outname), delete_layer = TRUE)
  }
}

mode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[base::which.max(tabulate(match(v, uniqv)))]
}

extract_one <- function(filename, shapefile_extractor,
                        use_varname = TRUE, varname,
                        prefix = prefix, s3_base = s3_base) {
  # function to extract all climate time series based on shapefile input
  # this results in large list of all months/years within the raster climate data
  # each list is written out to a csv so this only needs to be run once.
  # inputs:
  # filename -> a list of all tif filenames with full path
  # shapefile_extractor -> the shapefile (point or polygon) to extract climate data
  require(tidyverse)
  require(raster)
  
  if (use_varname == TRUE) {
    
    file_split <- filename %>%
      basename %>%
      strsplit(split = "_") %>%
      unlist
    year <- file_split[max(length(file_split))]
    dir_name <- dirname(filename)
    
    out_name <- paste0(dir_name, '/', varname, year)
    out_name <- gsub('.tif', '.csv', out_name)
    out_name
    
  } else {
    out_name <- gsub('.tif', '.csv', filename)
    out_name
    
  }
}

get_polygons <- function(decade) {
  
  if ( decade == '1990') {
    polygons <- urban_1990
  } else if ( decade == '2000') {
    polygons <- urban_2000
  } else if ( decade == '2010') {
    polygons <- urban_2010
  }
}

load_data <- function(url, dir, layer, outname) {
  file <- paste0(dir, "/", layer, ".shp")
  
  if (!file.exists(file)) {
    download.file(url, destfile = paste0(dir, ".zip"))
    unzip(paste0(dir, ".zip"),
          exdir = dir)
    unlink(paste0(dir, ".zip"))
    
  }
  name <- paste0(outname, "_shp")
  name <- sf::st_read(dsn = dir, layer = layer)
  name
}


# http://www.spatialanalytics.co.nz/post/2017/09/11/a-parallel-function-for-spatial-analysis-in-r/
# Paralise any simple features analysis.
st_par <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (class(split_results[[1]]) == 'list' ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else {
    result <- do.call(rbind, split_results)
  }
  
  # Return result
  return(result)
}

st_par_union <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    mclapply(function(x) sf_func(x), mc.cores = n_cores)
  
  # Combine results back together. Method of combining depends on the output from the function.
  if ( length(class(split_results[[1]]))>1 | class(split_results[[1]])[1] == 'list' ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else {
    result <- do.call("rbind", split_results)
  }
  
  # Return result
  return(result)
}

# Paralise any simple features analysis.
st_parallel <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    parallel::mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2){
    output_class <- output_class[2]
  }
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix"){
    result <- do.call("rbind", split_results)
    names(result) <- NULL
  } else if (output_class == "sfc") {
    result <- do.call("c", split_results)
    result <- sf_func(result) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions.
  } else if (output_class %in% c('list', 'sgbp') ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else if (output_class == "data.frame" | output_class == "tbl_df" | output_class == "tbl"){
    result <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  # Return result
  return(result)
}
# Helper functions --------------------------------------------------------
classify_wui <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  as.factor(ifelse(x == "Low_Dens_Intermix", "Intermix WUI",
                   ifelse(x == "Low_Dens_Interface", "Interface WUI",
                          ifelse(x == "Med_Dens_Intermix", "Intermix WUI",
                                 ifelse(x == "Med_Dens_Interface", "Interface WUI",
                                        ifelse(x == "High_Dens_Interface", "Interface WUI",
                                               ifelse(x == "High_Dens_Intermix", "Intermix WUI",
                                                      ifelse(x == "Very_Low_Dens_Veg", "VLD",
                                                             ifelse(x == "Uninhabited_Veg", "Wildlands",
                                                                    ifelse(x == "Med_Dens_NoVeg", "Med Urban",
                                                                           ifelse(x == "Low_Dens_NoVeg", "Low Urban",
                                                                                  ifelse(x == "High_Dens_NoVeg", "High Urban",
                                                                                         'Other'))))))))))))
}

# Helper functions --------------------------------------------------------



classify_bu_per_fire <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 10, "0 - 10",
         ifelse(x >= 10 & x < 50, "10 - 50",
                ifelse(x >= 50 & x < 250, "50 - 250",
                       ifelse(x >= 250 & x < 1000, "250 - 500",
                              "> 500"))))
}

get_month_max <- function(df) {
  stopifnot(nrow(df) == 1)
  seasons <- c("Fall", "Spring", "Summer", "Winter")
  vals <- unlist(df[, seasons])
  vals <- ifelse(is.na(vals), 0, vals)
  data.frame(max_season = seasons[vals == max(vals)])
}

#Extract Legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

classify_returninterval <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 2, "0 - 2",
         ifelse(x >= 2 & x < 5, "2 - 5",
                ifelse(x >= 5 & x < 7, "5 - 10",
                       "> 10")))
}

classify_occpyear <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 10, "0 - 25",
         ifelse(x >= 25 & x < 50, "25 - 50",
                ifelse(x >= 50 & x < 75, "50 - 75",
                       "> 75")))
}


classify_fire_size_sml <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 4, "Small",
         ifelse(x >= 4 & x < 500, "Large",
                ifelse(x > 500, "Very Large")))
}

classify_wuiburned <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(is.na(x), "0",
         ifelse(x >= 0.01 & x < 3, "0.01 - 2",
                ifelse(x >= 3 & x < 10, "3 - 10",
                       ifelse(x >= 11 & x < 20, "11 - 20",
                              ifelse(x >= 21 & x < 30, "21 - 30",
                                     "> 30")))))
}

classify_homesthreat <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x > 0 & x < 100, "0 - 100",
         ifelse(x >= 100 & x < 500, "100 - 500",
                ifelse(x >= 500 & x < 1000, "500 - 1000",
                       "> 1000")))
}

classify_homesthreat2 <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x > 0 & x < 10, 10,
         ifelse(x >= 10 & x < 100, 100,
                ifelse(x >= 100 & x < 500, 500,
                       ifelse(x >= 500 & x < 1000, 1000,
                              1001))))
}

classify_suppresscosts <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(is.na(x), 0,
         ifelse(x == 0, "0",
                ifelse(x >= 0.01 & x < 10000, "0.01 - 10k",
                       ifelse(x >= 10000 & x < 50000, "10k - 50k",
                              ifelse(x >= 50000 & x < 1000000, "50k - 1M",
                                     ifelse(x >= 1000000 & x < 10000000, "1M - 10M",
                                            ifelse(x >= 10000000 & x < 20000000, "10M - 20M",
                                                   ifelse(x >= 20000000 & x < 30000000, "20M - 30M",
                                                          ifelse(x >= 30000000 & x < 40000000, "30M - 40M",
                                                                 "> 40M")))))))))
  
}

theme_nothing <- function(base_size = 12, legend = FALSE){
  if(legend){
    theme(
      axis.text =          element_blank(),
      axis.title =         element_blank(),
      panel.background =   element_blank(),
      panel.grid.major =   element_blank(),
      panel.grid.minor =   element_blank(),
      axis.ticks.length =  unit(0, "cm"),
      panel.spacing =      unit(0, "lines"),
      plot.margin =        unit(c(0, 0, 0, 0), "lines")
    )
  } else {
    theme(
      line =               element_blank(),
      rect =               element_blank(),
      text =               element_blank(),
      axis.ticks.length =  unit(0, "cm"),
      legend.position =    "none",
      panel.spacing =      unit(0, "lines"),
      plot.margin =        unit(c(0, 0, 0, 0), "lines")
    )
  }
}


sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}


classify_suppresscosts2 <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(is.na(x), 0,
         ifelse(x >= 0.01 & x < 10000, 10000,
                ifelse(x >= 10000 & x < 50000, 50000,
                       ifelse(x >= 50000 & x < 1000000, 1000000,
                              ifelse(x >= 1000000 & x < 10000000, 10000000,
                                     ifelse(x >= 10000000 & x < 20000000, 20000000,
                                            ifelse(x >= 20000000 & x < 40000000, 40000000,
                                                   40000001)))))))
  
}

classify_new_categories <-  function(x) {
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

classify_seasons <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes
  ifelse(x >= 60 & x <= 153, "Spring",
         ifelse(x > 153 & x < 244, "Summer",
                ifelse(x > 244 & x < 335, "Fall",
                       "Winter")))
}

classify_seasons_bae <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes
  ifelse(x >= 12 & x <= 2, "Winter",
         ifelse(x > 3 & x < 5, "Spring",
                ifelse(x > 6 & x < 8, "Summer",
                       "Fall")))
}


classify_ptsize_breaks2 <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes
  ifelse(x >= 1 & x <= 25, .1,
         ifelse(x >= 26 & x <= 100, .3,
                ifelse(x >= 101 & x <= 300, .6,
                       ifelse(x >= 301 & x <= 700, .7,
                              1))))
}

bucket <- function(x,y) trunc(x/y)*y

classify_reclass_lev1 <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  as.factor(ifelse(x == "TROPICAL WET FORESTS", "Tropical Wet Forest",
                   ifelse(x == "GREAT PLAINS", "Great Plains",
                          ifelse(x == "EASTERN TEMPERATE FORESTS", "Eastern Temperate Forest",
                                 ifelse(x == "NORTH AMERICAN DESERTS", "North American Deserts",
                                        ifelse(x == "SOUTHERN SEMIARID HIGHLANDS", "Southern Semiarid Highlands",
                                               ifelse(x == "TEMPERATE SIERRAS", "Temperate Sierras",
                                                      ifelse(x == "MEDITERRANEAN CALIFORNIA", "Mediterranean",
                                                             ifelse(x == "NORTHWESTERN FORESTED MOUNTAINS", "Northwest Forested Mtns",
                                                                    ifelse(x == "MARINE WEST COAST FOREST", "Marine West Coast Forest",
                                                                           "Northern Forests"))))))))))
}


# GGPLOT Theme ------------------------------------------------------------
theme_pub <- function(base_size=11, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 13),
            
            panel.border = element_rect(colour = NA),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),
            
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            legend.title = element_text(size=11),
            legend.position = "right",
            legend.text = element_text(size=11),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "white"),
            
            strip.background=element_rect(colour=NA),
            strip.text.x = element_text(size = 10),
            
            axis.title = element_text(size = 11),
            axis.text.x = element_text(size = 10, angle = 65, hjust = 1),
            axis.text.y = element_text(size = 11)))
}
