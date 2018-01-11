# Clean ICS-209 from 2001-2013 -----------------------------

fam_rep <- fread(file.path(ics_famweb, "ics209_2001_2013_wfonlyv3.csv")) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

est_lookup <- fread(file.path(ics_famweb, "clean_estimated_costs.csv"))
latlong <- fread(file.path(ics_latlong, "ics209Incidents-cleaned_ll.csv"))

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
         est_final_costs = remove_dollar(clean_estimated_costs(incident_unique_id, est_final_costs)),
         costs_to_date_c = remove_dollar(clean_costs_to_date(incident_unique_id, costs_to_date)),
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
            lat = max(lat),
            long = min(long),
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
            home.damage = max(damaged_res),
            comm.destroyed = max(destroyed_comm),
            comm.threat = max(threatened_comm),
            comm.damage = max(damaged_comm),
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
         lat = ifelse(is.na(lat_c), lat.x, lat_c),
         long = ifelse(is.na(long_c), long.x, long_c),
         syear = syear.x,
         cause = cause.x) %>%
  dplyr::select(-lat_c, -long_c, -confi, -syear.y, -lat.y, -cause.x, -cause.y,
         -long.y, -lat.x, -long.x, -syear.x, -syear.y)
