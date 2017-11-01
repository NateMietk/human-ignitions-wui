source("src/functions/helper_functions.R")

x <- c("data.table", "tidyverse", "tidyverse", "magrittr", "sf",
       "assertthat", "purrr", "httr", "rvest", "lubridate", "parallel")
lapply(x, library, character.only = TRUE, verbose = FALSE)

## Download and process State data
# Creat directories for state data
prefix <- ifelse(Sys.getenv("LOGNAME") == "NateM", file.path("data"),
                 ifelse(Sys.getenv("LOGNAME") == "nami1114", file.path("data"),
                        file.path("../data")))
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_5m")
ics_prefix <- file.path(prefix, "ics209")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ics_prefix)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

ncores <- detectCores()

us_shp <- file.path(us_prefix, "cb_2016_us_state_5m.shp")
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_5m.zip"
  dest <- paste0(us_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = us_prefix)
  unlink(dest)
  assert_that(file.exists(us_shp))
}

usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_5m", quiet= TRUE) %>%
  st_par(., st_transform, n_cores = ncores, crs = "+proj=longlat +datum=WGS84") %>%  # e.g. US National Atlas Equal Area
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico",
                       "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands",
                       "American Samoa", "Guam"))) %>%
  mutate(group = 1)

# Clean ICS-209 from 2001-2013 -----------------------------
source("src/R/helper_functions.R")

fam_rep <- fread("data/ics209/input_tbls/famweb/ics209_2001_2013_wfonlyv3.csv") %>%
  mutate_all(funs(replace(., is.na(.), 0)))

est_lookup <- fread("data/ics209/input_tbls/famweb/clean_estimated_costs.csv")
latlong <- fread("data/ics209/input_tbls/latlong/ics209Incidents-cleaned_ll.csv")

names(fam_rep) %<>% tolower

fam <- fam_rep %>% 
  filter(!(un_ustate %in% c("AK", "HI", "PR"))) %>%
  filter(type_inc != "RX") %>%
  mutate(long = -longitude,
         lat = latitude,
         incidentnum = incident_number,
         incidentname = as.factor(incident_name),
         rdate = ymd(report_date),
         rdoy = yday(rdate),
         rmonth = month(rdate),
         rday = day(rdate),
         eyear = year(rdate),
         sdate = floor_date(ymd_hms(start_date), "day"),
         sdoy = yday(sdate),
         smonth = month(sdate),
         sday = day(sdate),
         syear = year(sdate),
         state = un_ustate,
         area_ha = ifelse(area_measurement == "SQ MILES", area*258.99903998855,
                          area*0.404686),
         area_km2 = area_ha*0.01,
         est_final_costs = dollarToNumber_vectorised(clean_estimated_costs(incident_unique_id, est_final_costs)),
         costs_to_date_c = dollarToNumber_vectorised(clean_costs_to_date(incident_unique_id, costs_to_date)),
         cause_binary = ifelse(cause == "H", "2", 
                               ifelse(cause == "O", "2", 
                                      ifelse(cause =="L", "1", "0"))),
         confi = "H") %>%
  left_join(est_lookup, by = c("incident_unique_id", "est_final_costs")) %>%
  mutate(est_final_costs_c = ifelse(is.na(est_final_costs_c), est_final_costs, est_final_costs_c),
         costs = ifelse(est_final_costs_c == 0 & costs_to_date_c > 1, costs_to_date_c,
                        est_final_costs_c))

fam_clean <- fam %>%
  group_by(incident_unique_id, eyear, state) %>%
  summarise(incidentnum = last(incidentnum),
            incidentname = last(incidentname),
            emonth = max(rmonth),
            eday = max(rday),
            edoy = max(rdoy),
            syear = ifelse(is.na(max(syear)), max(eyear), max(syear)),
            smonth = ifelse(is.na(min(smonth)), min(emonth), min(smonth)),
            sday = ifelse(is.na(min(sday)), min(eday), min(sday)),
            sdoy = ifelse(is.na(min(sdoy)), min(edoy), min(sdoy)),
            area_km2 = max(area_km2),
            costs = max(costs),
            fatalities = max(fatalities),
            home.destroyed = max(destroyed_res),
            home.threat = max(threatened_res),
            max.pers = max(imsr_total_personnel),
            max.aerial = max(imsr_num_aerial),
            tot.pers = sum(imsr_total_personnel),
            tot.aerial = sum(imsr_num_aerial),
            max.agency.support = max(imsr_num_agencies),
            cause = max(cause_binary),
            cause = ifelse(cause == "2", "Human", 
                           ifelse(cause =="1", "Lightning", "Unk")),
            confi = last(confi)) %>%
  left_join(., latlong, by = "incident_unique_id") %>%
  mutate(confidence = ifelse(is.na(confidence), confi, confidence),
         lat = ifelse(is.na(lat_c), lat, lat_c),
         long = ifelse(is.na(long_c), long, long_c)) %>%
  select(-lat_c, -long_c, -confi)

write_csv(fam_clean, path = "data/ics209/output_tbls/ics209_conus.csv")
