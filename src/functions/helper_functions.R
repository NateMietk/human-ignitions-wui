

# Helper functions --------------------------------------------------------

dollarToNumber_vectorised <- function(vector) {
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

clean_estimated_costs <- function(x, y, z) {
  # x = incident_unique_id
  # y = est_final_costs
  est_final_costs_c <-
    ifelse(x == "WA-OWF-199|2003|1"        & y > 39000000, 40000000,
           ifelse(x == "CO-LRX-329|2012|1"        & y > 39200000, 39200000,
                  ifelse(x == "MT-FNF-036|2003|1"        & y > 35000000, 37000000,
                         ifelse(x == "CA-SRF-1126|2008|1"       & y > 24000000, 24000000,
                                ifelse(x == "WA-OWF-271|2004|1"        & y >  42000000, 28000000,
                                       ifelse(x == "WA-OWF-271|2004|1"        & y >  42000000, 28000000,
                                              ifelse(x == "WA-OWF-199|2003|1"        & y > 40000000, 40000000,
                                                     ifelse(x == "WA-OWF-199|2003|1"        & y > 40000000, 40000000,
                                                            ifelse(x == "CA-KNF-5659|2012|1"       & y > 28000000, 28000000,
                                                                   ifelse(x == "CA-SRF-1131|2009|1"       & y > 20000000, 17500000,
                                                                          ifelse(x == "CA-SRF-997|2006|1"        & y > 20000000, 18000000,
                                                                                 ifelse(x == "CA-TMU-11011|2007|1"      & y >= 15000000, 13500000,
                                                                                        ifelse(x =="ID-BOF-000183|2006|1"      & y > 18000000, 18000000,
                                                                                               ifelse(x == "CA-SHF-002744|2012|1"     & y > 10000000, 10000000,
                                                                                                      ifelse(x == "AZ-CNF-090|2003|1"        & y > 16400000, 16400000,
                                                                                                             y)))))))))))))))
}

clean_costs_to_date <- function(x, y) {
  # x = incident_unique_id
  # y = est_final_costs

  costs_to_date_c <-
    ifelse(x == "CA-BTU-007660|2008|1"   & y == 876000000, 87600000,
           ifelse(x == "CA-STF-002857|2013|1"   & y == 1161000000, 116100000,
                  ifelse(x == "CA-CNF-002512|2013|1"   & y == 328900000 | y == 34885970 , 3200000,
                         ifelse(x == "UT-UIF-18044|2001|1"    & y == 293000000, 29300,
                                ifelse(x == "CA-ANF-2297|2013|1"     & y == 234000000 | y == 234100000, 23400000,
                                       ifelse(x == "NM-GNF-000230|2013|1"   & y == 143000000, 14300000,
                                              ifelse(x == "CA-STF-002857|2013|1"   & y == 1161000000, 116100000,
                                                     ifelse(x == "AZ-PNF-120266|2012|1"   & y == 122729904, 12272990,
                                                            ifelse(x == "CA-INF-801|2007|1"      & y == 100000000, 1000000,
                                                                   ifelse(x == "CA-BTU-0008116|2012|1"  & y == 170000000 | y == 97000000, 9700000,
                                                                          ifelse(x == "CA-BTU-0008116|2012|1"  & y == 870000000, 87000000,
                                                                                 ifelse(x == "CA-ENF-017646|2004|1"   & y == 77421200, 7742120,
                                                                                        ifelse(x == "CA-INF-801|2007|1"      & y == 100000000, 1000000,
                                                                                               ifelse(x == "CA-LNU -005495|2012|1"  & y == 11000000, 1000000,
                                                                                                      ifelse(x == "CA-MNF-894|2006|1"      & y == 14389638, 1438963,
                                                                                                             ifelse(x == "CA-MVU-011019|2011|1"   & y == 12180000, 1518000,
                                                                                                                    ifelse(x == "CA-RRU-056869|2003|1"   & y == 29060700, 2996070,
                                                                                                                           ifelse(x == "CA-RRU-56851|2011|1"    & y == 1000000, 100000,
                                                                                                                                  ifelse(x == "CA-SHF-002744|2012|1"   & y == 364000000, 36400000,
                                                                                                                                         ifelse(x == "CA-SHF-2521|2012|1"     & y == 36000000, 3600000,
                                                                                                                                                ifelse(x == "CA-VNC-03080298|2003|1" & y == 27500000, 27500000,
                                                                                                                                                       ifelse(x == "ID-CWF-050314|2005|1"   & y == 15000000, 1500000,
                                                                                                                                                              ifelse(x == "CA-MVU-014084|2013|1"   & y == 92000000, 9200000,
                                                                                                                                                                     ifelse(x == "WA-OWF-000616|2013|1"   & y == 85000000, 850000,
                                                                                                                                                                            ifelse(x == "CA-ENF-017646|2004|1"   & y == 77421200 | y == 77440000, 7740000,
                                                                                                                                                                                   ifelse(x == "MT-BRF-005432|2012|1"   & y == 77000000, 7700000,
                                                                                                                                                                                          ifelse(x == "WY-MB1-062|2002|1"      & y == 75000000, 7500000,
                                                                                                                                                                                                 ifelse(x == "AK-TAS-313401|2013|1"   & y == 70388044, 7038044,
                                                                                                                                                                                                        ifelse(x == "OR-MAF-012257|2012|1"   & y == 15847055 | y == 62000000, 6200000,
                                                                                                                                                                                                               ifelse(x == "WA-OWF-000530|2013|1"   & y == 53252222, 5352222,
                                                                                                                                                                                                                      ifelse(x == "WY-BTF-014|2002|1"      & y == 48860963, 4886963,
                                                                                                                                                                                                                             ifelse(x == "OR-UPF-120132|2012|1"   & y == 29981000, 2998100,
                                                                                                                                                                                                                                    ifelse(x == "ID-CMS-430012|2005|1"   & y == 29000000 | y == 28000000, 2800000,
                                                                                                                                                                                                                                           ifelse(x == "WA-OWF-398|2006|1"      & y == 68175390, 88175390,
                                                                                                                                                                                                                                                  ifelse(x == "CA-KNF-3536|2006|1"     & y == 27900000, 2790000,
                                                                                                                                                                                                                                                         ifelse(x == "NV-HTF-1365|2006|1"     & y == 14000000, 1400000,
                                                                                                                                                                                                                                                                ifelse(x == "AZ-TCA-130031|2013|1"   & y == 8300000, 830000,
                                                                                                                                                                                                                                                                       ifelse(x == "CA-BDU-11262|2003|1"    & y > 13000000, 13000000,
                                                                                                                                                                                                                                                                              y))))))))))))))))))))))))))))))))))))))
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
  if ( class(split_results[[1]]) == 'list' ){
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

# Helper functions --------------------------------------------------------
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

fit_mblm <- function(df) {
  # estimate intercept and slope
  # input:
  #  - df (data.frame): has columns FIRE_YEAR & N
  # return:
  #  - data.frame with intercept and slope
  y <- df$tot_fire
  x <- df$FIRE_YEAR
  m <- mblm(y ~ x, repeated = FALSE)
  tmp <- summary(m)
  data.frame(intercept = m$coefficients[1],
             slope = m$coefficients[2],
             pval = tmp$coefficients[8],
             Class = df$Class[1],
             IGNITION = df$IGNITION[1])
}

fit_mblm_bae <- function(df) {
  # estimate intercept and slope
  # input:
  #  - df (data.frame): has columns FIRE_YEAR & N
  # return:
  #  - data.frame with intercept and slope
  y <- df$bae
  x <- df$FIRE_YEAR
  m <- mblm(y ~ x, repeated = FALSE)
  tmp <- summary(m)
  data.frame(intercept = m$coefficients[1],
             slope = m$coefficients[2],
             pval = tmp$coefficients[8],
             Class = df$Class[1],
             IGNITION = df$IGNITION[1])
}

fit_mblm_fsl <- function(df) {
  # estimate intercept and slope
  # input:
  #  - df (data.frame): has columns FIRE_YEAR & N
  # return:
  #  - data.frame with intercept and slope
  y <- df$Fseas
  x <- df$FIRE_YEAR
  m <- mblm(y ~ x, repeated = FALSE)
  tmp <- summary(m)
  data.frame(intercept = m$coefficients[1],
             slope = m$coefficients[2],
             pval = tmp$coefficients[8],
             Class = df$Class[1],
             IGNITION = df$IGNITION[1])
}

fit_mblm_fpy <- function(df) {
  # estimate intercept and slope
  # input:
  #  - df (data.frame): has columns FIRE_YEAR & N
  # return:
  #  - data.frame with intercept and slope
  y <- df$FPY
  x <- df$FIRE_YEAR
  m <- mblm(y ~ x, repeated = FALSE)
  tmp <- summary(m)
  data.frame(intercept = m$coefficients[1],
             slope = m$coefficients[2],
             pval = tmp$coefficients[8],
             Class = df$Class[1],
             IGNITION = df$IGNITION[1])
}

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

classify_fire_size <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 3, "0 - 2",
         ifelse(x >= 3 & x < 11, "3 - 10",
                ifelse(x >= 11 & x < 100, "11 - 100",
                       "> 100")))
}

classify_fire_size_cl <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 0.4, "Very Small",
         ifelse(x >= 0.4 & x < 4, "Small",
                ifelse(x >= 4 & x < 40, "Medium",
                       "Large")))
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
                ifelse(x >= 0.01 & x < 5, "1 - 5",
                       ifelse(x >= 5 & x < 10, "5 - 10",
                              ifelse(x >= 10 & x < 20, "10 - 20",
                                     ifelse(x >= 20 & x < 30, "20 - 30",
                                            ifelse(x >= 30 & x < 40, "30 - 40",
                                                   ifelse(x >= 40 & x < 50, "40 - 50",
                                                          "> 50"))))))))

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
