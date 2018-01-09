
ics_209 <- wui_209 %>% 
  filter(cause != "Unk") %>%
  group_by(syear) %>%
  summarise(ICS_Fires = as.numeric(n()),
            ICS_Area = sum(area_km2),
            ICS_Totals = sum(costs)) %>%
  as.data.frame(.) %>%
  mutate(Year = as.integer(syear)) %>%
  dplyr::select(-geom, -syear) %>% as.tibble()

extraction <- extract_tables("https://www.nifc.gov/fireInfo/fireInfo_documents/SuppCosts.pdf")

# convert to df (not super general, but works for this particular case)
costs <- extraction[[1]][-c(1:2), ] %>%
  as_tibble() %>%
  separate(V2, into = c('Fires', 'Acres', 'Forest Service', 'DOI Agencies'), sep = ' {1,}') %>%
  rename(Year = V1, Total = V3) %>%
  mutate(Year = as.integer(remove_dollar(Year)),
         NIFC_Fires = remove_dollar(Fires),
         NIFC_Area = (remove_dollar(Acres)*0.00404686),
         NIFC_Total = remove_dollar(Total)) %>%
  dplyr::select(-`Forest Service` ,-`DOI Agencies`, -Fires, -Acres, -Total) %>%
  left_join(., ics_209, by = "Year") %>%
  filter(Year > 2000 & Year < 2014)
names(costs) %<>% tolower
  
costs_p <- costs %>%
  ggplot() +
  geom_point(aes(x = year, y = nifc_total/1000000000), color = "#1F77B4", size = 3) +
  geom_point(aes(x = year, y = ics_totals/1000000000), color = "#2CA02C", size = 3) +
  geom_line(aes(x = year, y = nifc_total/1000000000), color = "#1F77B4") +
  geom_line(aes(x = year, y = ics_totals/1000000000), color = "#2CA02C") +
  geom_smooth(aes(x = year, y = nifc_total/1000000000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75, color = "#1F77B4") +
  geom_smooth(aes(x = year, y = ics_totals/1000000000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75, color = "#2CA02C") +
  scale_x_continuous(breaks = seq(min(costs$year), max(costs$year), by = 2)) +
  theme_pub()  + 
  xlab("Year") + ylab("Fire Suppression Cost (in billions of dollars)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right")
ggsave(file = "CostCompare.eps", costs_p, width = 5, height = 4, dpi=1200)
