
# Prep raw data ----------------------------------------------
slim <- wuw_short %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class != "Urban") %>%
  mutate(fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         size_class = classify_fire_size(fire_size_km2),
         seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10),
         Months = classify_months(as.integer(DISCOVERY_DATE))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(US_L3NAME != "") 

ICS_red <- ICS[!duplicated(ICS$ICS_209_INCIDENT_NUMBER),]

wuw_ics209_cln <- ICS_red %>%
  mutate(costs = as.numeric(SuppressCo),
         Class = classify_new_categories(WUICLASS10)) %>%
  ungroup()

ICS_short <- wuw_ics209_cln[!is.na(wuw_ics209_cln$IGNITION),]

slim_209 <- wuw_ics209_cln %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class != "Urban") %>%
  mutate(fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         size_class = classify_fire_size(fire_size_km2),
         seasons = classify_seasons(DISCOVERY_DOY),
         Months = classify_months(as.integer(DISCOVERY_DATE))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(Region != "NA")

wuw_mtbs_cln <- wuw_mtbs %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class) %>%
  summarize(n_count = n(),
            a_sum = sum(AREA_km2)) %>%
  ungroup()

wuw_mtbs_short_dup <- wuw_mtbs_short[!duplicated(wuw_mtbs_short$Fire_ID),]


wuw_mtbs_short_cln <- wuw_mtbs_short %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class, IGNITION) %>%
  summarize(n_count = n(),
            a_sum = sum(AREA_km2)) %>%
  ungroup()

setwd("/Users/NateM/Dropbox/RScripts/WUIFire/WorkOut")


# Prep the data and group ***MTBS**** ------------------------------------------------

# Calculates based on the CLASSES and REGIONS
slim_thinyr_mtba <- wuw_mtbs_short %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(burn_area = sum(AREA_km2)) %>%
  ungroup() %>%
  filter(Class != "NA")

slim_thinyr_mtff <- wuw_mtbs_short_dup %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(fire_freq = n()) %>%
  ungroup() %>%
  filter(Class != "NA")


# Prep the data and group ***Short**** ------------------------------------------------

slim_thinyr_ba <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(burn_area = sum(fire_size_km2)) %>%
  ungroup()

slim_thinyr_ff <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(fire_freq = n()) %>%
  ungroup()

slim_thin_ff <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(Region, IGNITION, Class) %>%
  summarize(fire_freq = n()) %>%
  ungroup() %>%
  spread(IGNITION, fire_freq)

slim_thin_ba <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(Region, IGNITION,Class) %>%
  summarize(burn_area = sum(fire_size_km2)) %>%
  ungroup() %>%
  spread(IGNITION, burn_area)

# Calculates the Interquartile Range to determine the fire season length
slim_thin_IQR <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(Region, IGNITION, Class) %>%
  summarize(IQR_fs = IQR(as.integer(DISCOVERY_DOY))) %>%
  ungroup() %>%
  spread(IGNITION, IQR_fs) 


# Calculates based on the CLASSES
slim_thin_ff <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(IGNITION,Class) %>%
  summarize(fire_freq = n()) %>%
  ungroup() %>%
  spread(IGNITION, fire_freq)

slim_thin_ba <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(IGNITION,Class) %>%
  summarize(burn_area = sum(fire_size_km2)) %>%
  ungroup() %>%
  spread(IGNITION, burn_area)

# Calculates the Interquartile Range to determine the fire season length
slim_thin_IQR <- slim %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(IGNITION, Class) %>%
  summarize(IQR_fs = IQR(as.integer(DISCOVERY_DOY))) %>%
  ungroup() %>%
  spread(IGNITION, IQR_fs) 


slim_grouped <- slim %>%
  group_by(US_L3NAME,  STATE, Class, DISCOVERY_DOY, Months, seasons, IGNITION, fire_size_km2, size_class) %>%
  summarize(N = n()) %>%
  ungroup()

slim_season <- slim %>%
  group_by(Class, IGNITION, seasons) %>%
  summarize(n_count = n(),
            a_sum = sum(fire_size_km2)) %>%
  ungroup()

slim_seasonality <- slim %>%
  group_by(US_L3NAME, Class, IGNITION, seasons) %>%
  summarize(n_count = n()) %>%
  ungroup() %>%
  spread(seasons, n_count) %>%
  group_by(US_L3NAME, Class, IGNITION) %>%
  do(get_month_max(.))

# Prep the data and group ***ICS 209s**** ------------------------------------------------
#Time series
slim209_thinyr_cost <- slim_209 %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(sum_cost = sum(as.numeric(costs))) %>%
  ungroup() %>%
  filter(Class != "NA")

slim209_thinyr_home <- slim_209 %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(sum_home = sum(ResDestroy)) %>%
  ungroup() %>%
  filter(Class != "NA")

# Calculates based on the CLASSES and REGIONS
slim209_thin_cost <- slim_209 %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(Region, IGNITION, Class) %>%
  summarize(sum_cost = sum(as.numeric(costs))) %>%
  ungroup() %>%
  spread(IGNITION, sum_cost) %>%
  filter(Region != "NA")

slim209_thin_home <- slim_209 %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(Region, IGNITION,Class) %>%
  summarize(sum_home = sum(ResDestroy)) %>%
  ungroup() %>%
  spread(IGNITION, sum_home) %>%
  filter(Region != "NA")

slim209_thin_cost <- slim_209 %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Region = factor(Region, levels=c("East", "Central", "West"))) %>%
  group_by(IGNITION,Class) %>%
  summarize(sum_cost = sum(as.numeric(costs))) %>%
  ungroup() %>%
  spread(IGNITION, sum_cost)

slim209_thin_home <- slim_209 %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  group_by(IGNITION,Class) %>%
  summarize(sum_home = sum(ResDestroy)) %>%
  ungroup() %>%
  spread(IGNITION, sum_home)

slim_209_costs_class <-  slim_209 %>%
  group_by(Class, FIRE_YEAR, IGNITION) %>%
  summarize(Tot = sum(as.numeric(costs))) %>%
  ungroup() %>%
  spread(Class, Tot) %>%
  gather(key = Class, Costs, -IGNITION, -FIRE_YEAR)

slim_209_costs <-  slim_209 %>%
  group_by(FIRE_YEAR, IGNITION) %>%
  summarize(Tot = sum(as.numeric(costs))) %>%
  ungroup() %>%
  spread(IGNITION, Tot) %>%
  gather(key = IGNITION, Costs, -FIRE_YEAR)

slim_209_struct <-  wuw_209 %>%
  group_by(Class, IGNITION) %>%
  summarize(Tot = sum(as.numeric(structures))) %>%
  ungroup() %>%
  spread(Class, Tot) %>%
  mutate(Type = "Observed") %>%
  gather(key = TotCost, value, -IGNITION, -Type)

# Plots ***MTBS***  -----------------------------------------------------------------------
p1 <- slim_thinyr_mtff %>%
  ggplot(aes(x = FIRE_YEAR, y = fire_freq, color = IGNITION)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Ignition Frequency") +
  theme_pub()  +
  theme(legend.position="none") +
  facet_wrap( ~ Class, ncol=1, scales = "free")

p2 <- slim_thinyr_mtba %>%
  ggplot(aes(x = FIRE_YEAR, y = burn_area, color = IGNITION)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Burn area (km2)") +
  theme_pub()  +
  theme(legend.position="none") +
  facet_wrap( ~ Class, ncol=1, scales = "free")

grid.arrange(p1, p2, ncol =2)
# Plots ***Short***  -----------------------------------------------------------------------
p1 <- slim_thinyr_ff %>%
  ggplot(aes(x = FIRE_YEAR, y = fire_freq, color = IGNITION)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Ignition Frequency") +
  theme_pub()  +
  theme(legend.position="none")+
  facet_wrap( ~ Class, ncol=1, scales = "free")

p2 <- slim_thinyr_ba %>%
  ggplot(aes(x = FIRE_YEAR, y = burn_area, color = IGNITION)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Burn area (km2)") +
  theme_pub()  +
  theme(legend.position="none") +
  facet_wrap( ~ Class, ncol=1, scales = "free")
grid.arrange(p1, p2, ncol =2)

# Plots ***ICS 209s***  -----------------------------------------------------------------------

slim209_thinyr_cost %>%
  ggplot(aes(x = FIRE_YEAR, y = sum_cost, color = IGNITION)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Fire suppression costs") +
  theme_pub()  +
  theme(legend.position="none")+
  facet_wrap( ~ Class, ncol=1, scales = "free")

slim209_thinyr_home %>%
  ggplot(aes(x = FIRE_YEAR, y = sum_home, color = IGNITION)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Homes destroyed") +
  theme_pub()  +
  theme(legend.position="none")+
  facet_wrap( ~ Class, ncol=1, scales = "free")

p100 <- slim_season %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  ggplot(aes(x = seasons, y = a_sum, fill = IGNITION)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  #scale_color_manual(values=c("red", "blue")) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(name="Season", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme(legend.position="top") +
  xlab("Seasonal Discovery Date") + ylab("Total fire size (km2)") +
  facet_wrap( ~ Class, ncol=1) +
  theme_pub()

p101 <- slim_season %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  ggplot(aes(x = seasons, y = n_count, fill = IGNITION)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  #scale_color_manual(values=c("red", "blue")) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(name="Season", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme(legend.position="top") +
  xlab("Seasonal Discovery Date") + ylab("Number of fire IGNITIONs") +
  facet_wrap( ~ Class, ncol=1) +
  theme_pub()
g <- arrangeGrob(p101, p100, nrow=1)
ggsave("SeasonalDiscoveryDate_FF_AB.jpg",g,width = 8, height = 8, dpi=1200)

p102 <- slim_thin %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = IGNITION, y = a_sum, fill = IGNITION)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  #scale_color_manual(values=c("red", "blue")) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(name="Season", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme(legend.position="top") +
  xlab("Seasonal Discovery Date") + ylab("Number of fire IGNITIONs") +
  facet_wrap( ~ Class, ncol=1) +
  theme_pub()
g <- arrangeGrob(p101, p100, nrow=1)


p2 <- slim_grouped %>% 
  filter(IGNITION == "Human") %>%
  transform(Class = factor(Class, levels=c("WUI","VLD", "Wildlands"))) %>%
  ggplot(aes(DISCOVERY_DOY, fill=IGNITION)) + 
  geom_density(color="red",fill="red") +
  #coord_flip() +
  #scale_y_reverse(limits = c(0.011, 0),breaks=seq(0,0.01, by = 0.005)) +
  scale_y_continuous(limits = c(0, 0.011),breaks=seq(0,0.01, by = 0.005)) +
  theme(text=element_text(size=10)
        ,legend.position="none") + 
  facet_wrap( ~ Class, scales= "free", ncol=1) +
  ylab("") + xlab("Julian Discovery Date") + ggtitle("Human IGNITIONs") +
  theme_pub()

p3 <- slim_grouped %>% 
  filter(IGNITION == "Lightning") %>%
  transform(Class = factor(Class, levels=c("WUI","VLD", "Wildlands"))) %>%
  ggplot(aes(DISCOVERY_DOY, fill=IGNITION)) + 
  geom_density(color="blue",fill="blue") +
  #coord_flip() +
  scale_y_continuous(limits = c(0, 0.011),breaks=seq(0,0.01, by = 0.005)) +
  theme(text=element_text(size=10)
        ,legend.position="none") + 
  facet_wrap( ~ Class, scales= "free", ncol=1) +
  ylab("") + xlab("Julian Discovery Date") + ggtitle("Lightning IGNITIONs") +
  theme_pub()

limits <- c(0, 365)
breaks <- seq(limits[1], limits[2], by=100)

# assign common axis to both plots
#p1.common.y <- p1 + scale_y_continuous(limits=limits, breaks=breaks)
p2.common.y <- p2 + scale_x_continuous(limits=limits, breaks=breaks)
p3.common.y <- p3 + scale_x_continuous(limits=limits, breaks=breaks)

# At this point, they have the same axis, but the axis lengths are unequal, so ...

# build the plots 
p1.common.y <- ggplot_gtable(ggplot_build(p102))
p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))
p3.common.y <- ggplot_gtable(ggplot_build(p3.common.y))

# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
p3.common.y$heights <- p1.common.y$heights

grid.arrange(p2.common.y, p1, p3.common.y, ncol=3, widths=c(3,6,3))
g <- arrangeGrob(p2.common.y, p1, p3.common.y, ncol=3, widths=c(3,6,3)) #generates g
ggsave(file = "DiscDate_wDenisty.jpg", g, width = 12, height = 7, dpi=1200) #saves g


# Seasonal suppresion costs and discovery date density  ----------------------------------------------
# Plot histograms based on discovery date and class
p1 <- slim %>% 
  mutate(seasons = classify_seasons(DISCOVERY_DOY)) %>%
  group_by(Class, IGNITION, seasons) %>%
  summarise(sumSize = (sum(fire_size_km2))) %>%
  transform(seasons = factor(seasons, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter","Spring","Summer", "Fall"))) %>%
  ggplot(aes(x = seasons, y = sumSize)) +
  geom_bar(aes(x = seasons, y = sumSize, fill = IGNITION), 
           position="dodge", stat="identity", color = "black") +
  scale_fill_manual(values=c("light gray", "gray40")) +
  theme(legend.position="top") +
  xlab("Julian Discovery Date") + ylab("Total Burned Area (km2)") +
  ggtitle("Fire totals per season")+
  facet_wrap( ~ Class,  ncol=1) +
  theme_pub()

p2 <- slim_grouped %>% 
  group_by(Class, IGNITION, seasons) %>%
  summarise(sumCosts = n()) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter","Spring","Summer", "Fall"))) %>%
  ggplot(aes(x = seasons, y = sumCosts)) +
  geom_bar(aes(x = seasons, y = sumCosts, fill = IGNITION), 
           position="dodge", stat="identity", color = "black") +
  scale_fill_manual(values=c("light gray", "gray40")) +
  theme(legend.position="top") +
  xlab("Julian Discovery Date") + ylab("Fire Frequency") +
  ggtitle("Fire frequency per season")+
  facet_wrap( ~ Class, ncol=1) +
  theme_pub()

p3 <- slim_209 %>% 
  mutate(seasons = classify_seasons(DISCOVERY_DOY)) %>%
  group_by(Class, IGNITION, seasons) %>%
  summarise(sumCosts = sum(SuppressCo)/1000000) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter","Spring","Summer", "Fall"))) %>%
  ggplot(aes(x = seasons, y = sumCosts)) +
  geom_bar(aes(x = seasons, y = sumCosts, fill = IGNITION), 
           position="dodge", stat="identity", color = "black") +
  scale_fill_manual(values=c("light gray", "gray40")) +
  theme(legend.position="top") +
  xlab("Julian Discovery Date") + ylab("Fire Suppression Costs ($; 10e6)") +
  ggtitle("Seasonal fire suppresion cost")+
  facet_wrap( ~ Class, ncol=1) +
  theme_pub()

p4 <- slim_209 %>% 
  mutate(seasons = classify_seasons(DISCOVERY_DOY)) %>%
  group_by(Class, IGNITION, seasons) %>%
  summarise(sumCosts = sum(ResDestroy)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter","Spring","Summer", "Fall"))) %>%
  ggplot(aes(x = seasons, y = sumCosts)) +
  geom_bar(aes(x = seasons, y = sumCosts, fill = IGNITION), 
           position="dodge", stat="identity", color = "black") +
  scale_fill_manual(values=c("light gray", "gray40")) +
  theme(legend.position="top") +
  xlab("Julian Discovery Date") + ylab("Total structures lost") +
  ggtitle("Seasonal structures lost")+
  facet_wrap( ~ Class, ncol=1) +
  theme_pub()
# assign common axis to both plots
p1.common.y <- p1 #+ scale_x_continuous(limits=limits, breaks=breaks)
p2.common.y <- p2 #+ scale_x_continuous(limits=limits, breaks=breaks)
p3.common.y <- p3 #+ scale_x_continuous(limits=limits, breaks=breaks)
p4.common.y <- p4 #+ scale_x_continuous(limits=limits, breaks=breaks)


# build the plots 
p1.common.y <- ggplot_gtable(ggplot_build(p1.common.y))
p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))
p3.common.y <- ggplot_gtable(ggplot_build(p3.common.y))
p4.common.y <- ggplot_gtable(ggplot_build(p4.common.y))


# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
p3.common.y$heights <- p1.common.y$heights
p4.common.y$heights <- p1.common.y$heights

g <- arrangeGrob(p1.common.y, p2.common.y, ncol=2, widths=c(5,5)) #generates g
ggsave(file = "Seasonal_freqtots.jpg", g, width=8, height = 5,  dpi=1200) #saves g

g <- arrangeGrob(p3, p4, ncol=2, widths=c(5,5)) #generates g
ggsave(file = "Seaonal_209.jpg", g, width=8, height = 5,  dpi=1200) #saves g









# Observed Vs Expected figures ----------------------------------------------

setwd("/Users/NateM/Dropbox/RScripts/WUIFire/Data/ObsVsExp")
obsexp_num <- read.csv("Class_FNum_ObsvsExp.csv", header = TRUE) 
obsexp_sum <- read.csv("Class_FSum_ObsvsExp.csv", header = TRUE) 

obsexp_short <- read.csv("Short_HLC_ObsExp.csv", header = TRUE) 
obsexp_mtbsshort <- read.csv("MTBSShort_HLC_ObsExp.csv", header = TRUE) 

p3 <- obsexp_short %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ObsExp = factor(ObsExp, levels=c("Observed", "Expected"))) %>%
  ggplot(aes(x = IGNITION, y = n_fire, fill = ObsExp)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  scale_fill_manual(values=c("light gray", "gray40")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("IGNITION Type") + ylab("Number of fire IGNITIONs") +
  facet_wrap( ~ Class) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

p4 <- obsexp_short %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ObsExp = factor(ObsExp, levels=c("Observed", "Expected"))) %>%
  ggplot(aes(x = IGNITION, y = sum_fire, fill = ObsExp)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  scale_fill_manual(values=c("light gray", "gray40")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("IGNITION Type") + ylab("Total burned area (km2)") +
  facet_wrap( ~ Class) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

p5 <- obsexp_mtbsshort %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ObsExp = factor(ObsExp, levels=c("Observed", "Expected"))) %>%
  ggplot(aes(x = IGNITION, y = n_fire, fill = ObsExp)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  scale_fill_manual(values=c("light gray", "gray40")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("IGNITION Type") + ylab("Number of fire IGNITIONs") +
  facet_wrap( ~ Class) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file = "FireNum_MTBS_Short_ObsVsExp.pdf", p5, width=5, height = 6,  dpi=1200) #saves g

p6 <- obsexp_mtbsshort %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ObsExp = factor(ObsExp, levels=c("Observed", "Expected"))) %>%
  ggplot(aes(x = IGNITION, y = sum_fire, fill = ObsExp)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  scale_fill_manual(values=c("light gray", "gray40")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("IGNITION Type") + ylab("Total burned area (km2)") +
  facet_wrap( ~ Class) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file = "FireSum_MTBS_Short_ObsVsExp.pdf", p6, width=5, height = 6,  dpi=1200) #saves g

p1 <- obsexp_num %>%
  filter(seasons != "Fall" & seasons != "Winter") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ObsExp = factor(ObsExp, levels=c("Observed", "Expected"))) %>%
  transform(seasons = factor(seasons, levels=c("Spring", "Summer"))) %>%
  ggplot(aes(x = seasons, y = NumFire, fill = ObsExp)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  scale_fill_manual(values=c("light gray", "gray40")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("Seasonal Discovery Date") + ylab("Number of fire IGNITIONs") +
  facet_grid(Class ~ IGNITION) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file = "FireNum_ObsVsExp.pdf", p1, width=5, height = 6,  dpi=1200) #saves g

p2<- obsexp_sum %>%
  filter(seasons != "Fall" & seasons != "Winter") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ObsExp = factor(ObsExp, levels=c("Observed", "Expected"))) %>%
  transform(seasons = factor(seasons, levels=c("Spring", "Summer"))) %>%
  ggplot(aes(x = seasons, y = FireSize, fill = ObsExp)) +
  geom_bar(position="dodge", stat="identity", color = "black") + 
  #scale_fill_manual(values=c("cadetblue3", "burlywood")) +
  scale_fill_manual(values=c("light gray", "gray40")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("Seasonal Discovery Date") + ylab("Total burned area (km2)") +
  facet_grid(Class ~ IGNITION) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file = "FireSum_ObsVsExp.pdf", p2, width=5, height = 6,  dpi=1200) #saves g

# Chi Sq tests ------------------------------------------------------------

FNumber <- obsexp_num %>%
  filter(seasons != "Fall" & seasons != "Winter") %>%
  spread(ObsExp, NumFire) %>%
  rowwise() %>%
  mutate(
    test_stat = chisq.test(c(Observed, Expected))$statistic,
    p_val = round(chisq.test(c(Observed, Expected))$p.value,4)
  )

TotFire <- obsexp_sum %>%
  filter(seasons != "Fall" & seasons != "Winter") %>%
  spread(ObsExp, FireSize) %>%
  rowwise() %>%
  mutate(
    test_stat = chisq.test(c(Observed, Expected))$statistic,
    p_val = round(chisq.test(c(Observed, Expected))$p.value, 4)
  )
