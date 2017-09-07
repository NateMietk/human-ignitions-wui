setwd("~/Dropbox/Professional/RScripts/Mietkiewicz_et_al_2017_WUI_Ignitions/results/Percentages/")

# Data Prep --> Fire Frequency --------
  wuw_short_cln <- wuw_short %>%
    mutate(fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
           size_class = classify_fire_size(fire_size_km2),
           seasons = classify_seasons(DISCOVERY_DOY),
           Class = classify_new_categories(WUICLASS10)) %>%
    filter(Class != "Urban")%>%
    left_join(., econorm, by = "US_L3NAME") 
  
  
  short_sums <- wuw_short_cln %>%
    filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
    group_by(IGNITION, Class) %>%
    summarize(tot_ign = n())
  
  wuw_short_cln_reg <- wuw_short %>%
    mutate(region = ifelse(Region == "North East", "East",
                           ifelse(Region == "South East", "East",
                                  ifelse(Region == "West", "West",
                                         "Central"))),
           fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
           size_class = classify_fire_size(fire_size_km2),
           seasons = classify_seasons(DISCOVERY_DOY),
           Class = classify_new_categories(WUICLASS10)) %>%
    filter(Class != "Urban")%>%
    left_join(., econorm, by = "US_L3NAME") 
  
  
  short_sums_reg <- wuw_short_cln_reg %>%
    filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
    group_by(region, IGNITION, Class) %>%
    summarize(tot_ign = n())

# Data Prep --> Burned Area Estimated --------
  bae_cln <- bae %>%
    mutate(AREA_km2 = as.numeric(AREA_km2),
           Class = classify_new_categories(WUICLASS10)) %>%
    ungroup() %>%
    filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")
  
  bae_sums <- bae_cln %>%
    filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
    group_by(IGNITION, Class) %>%
    summarize(tot_ign = n(),
              sum_ign = sum(AREA_km2))
  
  bae_cln_reg <- bae %>%
    mutate(region = ifelse(Region == "North East", "East",
                           ifelse(Region == "South East", "East",
                                  ifelse(Region == "West", "West",
                                         "Central"))),
           AREA_km2 = as.numeric(AREA_km2),
           Class = classify_new_categories(WUICLASS10)) %>%
    ungroup() %>%
    filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")
  
  bae_sums_reg <- bae_cln_reg %>%
    filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
    group_by(region, IGNITION, Class) %>%
    summarize(tot_ign = n(),
              sum_ign = sum(AREA_km2))
# Data Prep --> Costs, Homes Destroyed, Homes Threatened, and Fatalities -------------------------------------------------------------------
    
wuw_ics209_cln <- ICS %>%
  mutate(SuppressCo = as.numeric(SuppressCo),
         ResDestroy = as.numeric(ResDestroy),
         ResThreat = as.numeric(ResThreat),
         Fatalities = as.numeric(Fatalities),
         Injuries = as.numeric(Injuries),
         Class = classify_new_categories(WUICLASS10)) %>%
   ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")
  
ics209_sums <- wuw_ics209_cln %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  group_by(Ignition, Class) %>%
  summarize(Class_Costs = sum(SuppressCo),
            Class_ResD = sum(ResDestroy),
            Class_ResT = sum(ResThreat),
            Class_Fat = sum(Fatalities)) 

wuw_ics209_cln_reg <- ICS %>%
  mutate(region = ifelse(Region == "North East", "East",
                         ifelse(Region == "South East", "East",
                                ifelse(Region == "West", "West",
                                       "Central"))),
         SuppressCo = as.numeric(SuppressCo),
         ResDestroy = as.numeric(ResDestroy),
         ResThreat = as.numeric(ResThreat),
         Fatalities = as.numeric(Fatalities),
         Injuries = as.numeric(Injuries),
         Class = classify_new_categories(WUICLASS10)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

ics209_sums_reg <- wuw_ics209_cln_reg %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  group_by(region, Ignition, Class) %>%
  summarize(Class_Costs = sum(SuppressCo),
            Class_ResD = sum(ResDestroy),
            Class_ResT = sum(ResThreat),
            Class_Fat = sum(Fatalities)) 

# Plot Physical and Social Totals per region -----------------------------------------

p3 <- short_sums_reg %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = tot_ign/100000, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  theme(legend.position="top") +
  xlab("") + ylab("Fire frequency (in hundreds of thousands)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text.x = element_blank(),
        legend.position="none") +
  facet_wrap(~ region, nrow = 3)

p4 <- bae_sums_reg %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = sum_ign/100000, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  xlab("") + ylab("Burned landcover class (in hundreds of thousands; km2)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text.x = element_blank(),
        legend.position="none") +
  facet_wrap(~ region, nrow = 3)

p5 <- ics209_sums_reg %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_Costs/1000000000, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  xlab("") + ylab("Total accrued costs (in billions of dollars)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text.x = element_blank(),
        legend.position="none") +
  facet_wrap(~ region, nrow = 3)

p6 <- ics209_sums_reg %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_ResD/1000, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  xlab("") + ylab("Total structures destroyed (in thousands)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text.x = element_blank(),
        legend.position="none") +
  facet_wrap(~ region, nrow = 3)

p7 <- ics209_sums_reg %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_ResT/100000, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  xlab("") + ylab("Total structures threatened (in hundreds of thousands)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text.x = element_blank(),
        legend.position="none") +
  facet_wrap(~ region, nrow = 3)

p8 <- ics209_sums_reg %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_Fat/100, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  xlab("") + ylab("Total lives lost (in hundreds)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        legend.position="none") +
  facet_wrap(~ region, nrow = 3, strip.position = "right")

# Save figures to PNG -----------------------------------------------------

grid.arrange(p3, p4, p5, p6, p7, p8, ncol=6)
g <- arrangeGrob(p3, p4, p5, p6, p7, p8, ncol=6) #generates g
ggsave(file = "PhysicalVsSocialTotals_Regional.pdf", g, width =17, height = 8, dpi=1200, scale = 2, units = "cm") #saves g
ggsave(file = "PhysicalVsSocialTotals_Regional.EPS", g, width = 17, height = 8, dpi=1200, scale = 2, units = "cm") #saves g

# Plot Physical and Social Totals -----------------------------------------
  
p3 <- short_sums %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = tot_ign/100000, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  
  theme(legend.position="top") +
  xlab("") + ylab("Fire frequency (in hundreds of thousands)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none")

p4 <- bae_sums %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = sum_ign/100000, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  
  xlab("") + ylab("Burned landcover class (in hundreds of thousands; km2)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none")

p5 <- ics209_sums %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_Costs/1000000000, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  
  xlab("") + ylab("Total accrued costs (in billions of dollars)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none")

p6 <- ics209_sums %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_ResD/1000, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  
  xlab("") + ylab("Total structures destroyed (in thousands)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none")

p7 <- ics209_sums %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_ResT/100000, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  
  xlab("") + ylab("Total structures threatened (in hundreds of thousands)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none")

p8 <- ics209_sums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_Fat/100, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  
  xlab("") + ylab("Total lives lost (in hundreds)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position="none")

# Save figures to PNG -----------------------------------------------------

grid.arrange(p3, p4, p5, p6, p7, p8, ncol=6)
g <- arrangeGrob(p3, p4, p5, p6, p7, p8, ncol=6) #generates g
ggsave(file = "./results/Percentages/PhysicalVsSocialTotals.pdf", g, width =12, height = 5, dpi=1200) #saves g

# Trends ------------------------------------------------------------------
YearlyTrends <- wuw_ics209_cln %>%
  group_by(Year, Ignition, Class) %>%
  summarize(Class_Costs = sum(SuppressCo),
            Class_ResD = sum(ResDestroy),
            Class_ResT = sum(ResThreat))

coststrends <- YearlyTrends %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = Year)) +
  geom_point(aes(y = Class_Costs/100000000), color = "#1F77B4", size = 3) +
  geom_line(aes(y = Class_Costs/100000000), color = "#1F77B4", alpha = 0.5) +
  geom_smooth(aes(y = Class_Costs/100000000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75, color = "#1F77B4")+
  xlab("Year") + ylab("Fire suppression costs (in hundreds of millions)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right") +
  facet_grid(Ignition ~ Class)

hometrends <- YearlyTrends %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = Year)) +
  geom_point(aes(y = Class_ResD/1000), color = "#1F77B4", size = 2) +
  geom_point(aes(y = Class_ResT/10000), color = "#D62728", size = 2) +
  geom_line(aes(y = Class_ResD/1000), color = "#1F77B4", alpha = 0.5) +
  geom_line(aes(y = Class_ResT/10000), color = "#D62728", alpha = 0.5) +
  geom_smooth(aes(y = Class_ResD/1000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75, color = "#1F77B4")+
  geom_smooth(aes(y = Class_ResT/10000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75, color = "#D62728") + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*1, name = "Homes threatened (in tens of thousands)")) +
  xlab("Year") + ylab("Total homes destroyed (in thousands)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right") +
  facet_grid(Ignition ~ Class)



