
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


classify_fire_size_cl <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 0.25, "< 10 ha",
         ifelse(x >= 0.25 & x < 4, "10 - 400 ha",
           ifelse(x >= 4 & x < 50, "400 - 5000 ha",
                  ifelse(x >= 50 & x < 250, "5000 - 20000 ha", "> 20000 ha"))))
}

clean_class <- function(x, y) {
  if_else(x %in% c("CA-BTU-007660|2008|1","MT-CES-000122|2012|1","WA-OWF-000346|2010|1","CO-PSF-000429|2013|1","CA-BDU-006570|2006|1",
                   "CA-RRU-080142|2012|1","CA-SHF-1057|2008|1","WA-OWF-000583|2012|1","CA-SRF-1120|2008|1", "CA-HUU-003384|2008|1",
                   "CA-LNF-3923|2009|1","MT-MCD-000035|2012|1","CA-LNU-006610|2003|1","CA-TGU-4245|2008|1","WA-WEF-709|2001|1",
                   "CA-MMU-008107|2008|1"), "VLD",
          if_else(x %in% c("OR-UPF-0000034|2003|1","CA-BDF-5749|2006|1","NV-HTF-3364|2004|1","OR-71S-044|2002|1","AZ-KNF-00872|2009|1",
                           "CA-RRU-062519|2005|1","AZ-CNF-074|2007|1","NV-HTF-1111|2006|1","CA-MVU-1024|2002|1","ID-SCF-000003|2005|1",
                           "OR-SIF-003|2002|1"), "Wildlands",
                  y))
}

# Helper functions --------------------------------------------------------

remove_dollar <- function(vector) {
  # Want the vector as character rather than factor while
  # we're doing text processing operations
  vector <- as.character(vector)
  vector <- gsub("(\\$|,)","", vector)
  # Create a numeric vector to store the results in, this will give you
  # warning messages about NA values being introduced because the " K" values
  # can't be converted directly to numeric
  result <- as.numeric(vector)
  # Find all the "$N K" values, and modify the result at those positions
  k_positions <- grep(" K", vector)
  result[k_positions] <- as.numeric(gsub(" K","", vector[k_positions])) * 1000
  # Same for the "$ M" value
  m_positions <- grep(" M", vector)
  result[m_positions] <- as.numeric(gsub(" M","", vector[m_positions])) * 1000000
  return(result)
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
# Helper functions --------------------------------------------------------
classify_wui <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  as.factor(ifelse(x == "Low_Dens_Intermix", "Interface WUI",
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

classify_bu <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 25, "0 - 25",
         ifelse(x >= 25 & x < 250, "25 - 250",
                ifelse(x >= 250 & x < 1000, "250 - 1000",
                       ifelse(x >= 1000 & x < 10000, "1000 - 10000",
                              "> 10000"))))
}

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
         ifelse(x >= 0.01 & x < 10000, "0.01 - 10,000",
                ifelse(x >= 10000 & x < 50000, "10,000 - 50,000",
                       ifelse(x >= 50000 & x < 1000000, "50,000 - 1,000,000",
                              ifelse(x >= 1000000 & x < 10000000, "1,000,000 - 10,000,000",
                                     ifelse(x >= 10000000 & x < 20000000, "10,000,000 - 20,000,000",
                                            ifelse(x >= 20000000 & x < 30000000, "20,000,000 - 30,000,000",
                                                   ifelse(x >= 30000000 & x < 40000000, "30,000,000 - 40,000,000",
                                                          "> 40,000,001"))))))))

}

classify_pctbae <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(is.na(x), 0,
         ifelse(x < 1, "< 1",
                ifelse(x >= 0.01 & x < 10, "1 - 10",
                       ifelse(x >= 10 & x < 20, "10 - 20",
                              ifelse(x >= 20 & x < 30, "20 - 30",
                                     ifelse(x >= 30 & x < 40, "30 - 40",
                                            ifelse(x >= 40 & x < 50, "40 - 50",
                                                   "> 50")))))))

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

classify_ptsize_breaks <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes
  ifelse(x >= 1 & x <= 25, "1 - 25",
         ifelse(x >= 26 & x <= 100, "26 - 100",
                ifelse(x >= 101 & x <= 300, "101 - 300",
                       ifelse(x >= 301 & x <= 700, "301 - 700",
                              "> 700"))))
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
