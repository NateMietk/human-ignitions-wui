
# Summarize Ecoregion information using dplyr ---------------------------------------------------
wuw_eco <- wuw_short %>%
  mutate(fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         size_class = classify_fire_size(fire_size_km2),
         seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(US_L3NAME != "") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  

eco_ign_freq <- wuw_eco %>%
  group_by(US_L3NAME) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

eco_sum <- wuw_eco %>%
  group_by(IGNITION) %>%
  summarize(tot_fire = n(),
            sum_fire = sum(fire_size_km2)) %>%
  ungroup() 

eco_freq <- wuw_eco %>%
  group_by(US_L3NAME, IGNITION) %>%
  summarize(tot_fire = n(),
            sum_fire = round(sum(fire_size_km2),2)) %>%
  ungroup() 

Szeco_freq <- wuw_eco %>%
  filter(size_class != "Medium") %>%
  group_by(US_L3NAME, IGNITION) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

Seasoneco_freq <- wuw_eco %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  group_by(US_L3NAME, IGNITION) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

Eco_Cnt <- wuw_eco %>%
  group_by(US_L3NAME, IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., eco_freq, by = c("US_L3NAME", "IGNITION")) %>%
  mutate( us_l3name = US_L3NAME,
          fire_freq = n_fire*0.001,
          fire_freq = ifelse(fire_freq == 0, 0, fire_freq),
          tot_pct_freq = (n_fire/tot_fire)*100)
EcoCnt_Df <- left_join(er_df, Eco_Cnt, by = "us_l3name")

Eco_Pct <- wuw_eco %>%
  group_by(US_L3NAME, EcReg_Area_km2.x,IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., eco_ign_freq, by = "US_L3NAME") %>%
  spread(IGNITION, n_fire) %>%
  mutate(Lightning = ifelse(is.na(Lightning), 0, Lightning), 
         HU_PCT = ((Human/tot_fire)*100),
         LG_PCT = ((Lightning/tot_fire)*100),
         us_l3name = US_L3NAME)
Eco_Df <- left_join(er_df, Eco_Pct, by = "us_l3name")

EcoSzCl_Pct <- wuw_eco %>%
  group_by(US_L3NAME, IGNITION, Class, size_class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., Szeco_freq, by = c("US_L3NAME", "IGNITION")) %>%
  spread(IGNITION, n_fire) %>%
  mutate(Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Human = ifelse(is.na(Human), 0, Human),
         HU_PCT = ((Human/tot_fire)*100),
         LG_PCT = ((Lightning/tot_fire)*100),
         us_l3name = US_L3NAME)
EcoSzCl_Df <- left_join(er_df, EcoSzCl_Pct, by = "us_l3name")
EcoSzCl_Df <- na.omit(EcoSzCl_Df)

EcoSeason_Pct <- wuw_eco %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  group_by(US_L3NAME, IGNITION, Class, seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., Seasoneco_freq, by = c("US_L3NAME", "IGNITION")) %>%
  spread(IGNITION, n_fire) %>%
  mutate(Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Human = ifelse(is.na(Human), 0, Human),
         HU_PCT = ((Human/tot_fire)*100),
         LG_PCT = ((Lightning/tot_fire)*100),
         us_l3name = US_L3NAME)
EcoSeason_Df <- left_join(er_df, EcoSeason_Pct, by = "us_l3name")

# Plotting Ecoregions  ----------------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion")

colourCount = length(unique(bucket(EcoCnt_Df$tot_pct_freq, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

p1.0 <-  EcoCnt_Df %>%
  filter(tot_pct_freq != "NA") %>%
  filter(tot_pct_freq >= 1) %>%
  filter(IGNITION == "Human") %>%
  mutate(buckets = bucket(tot_pct_freq, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1) +
  coord_equal() + 
  #scale_fill_brewer(name="Num of Fire\n (in thousands)", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Num of Fire\n (in thousands)") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ IGNITION, switch = "y")

colourCount2 = length(unique(bucket(EcoCnt_Df$tot_pct_freq, 10)))
getPalette2 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p1.01 <-  EcoCnt_Df %>%
  filter(tot_pct_freq != "NA") %>%
  filter(tot_pct_freq >= 1) %>%
  filter(IGNITION == "Lightning") %>%
  mutate(buckets = bucket(tot_pct_freq, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1) +
  coord_equal() + 
  #scale_fill_brewer(name="Num of Fire\n (in thousands)", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Num of Fire\n (in thousands)") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ IGNITION, switch = "y")

grid.arrange(p1.0, p1.01, ncol = 2)

p1.1 <- Eco_Df %>%
  filter(HU_PCT != "NA") %>%
  filter(HU_PCT != 0) %>%
  mutate(buckets = bucket(HU_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p1.2 <- Eco_Df %>%
  filter(LG_PCT != "NA") %>%
  filter(LG_PCT != 0) %>%
  mutate(buckets = bucket(LG_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

colourCount = length(unique(bucket(EcoSzCl_Df$HU_PCT, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

p2 <- EcoSzCl_Df %>%
  filter(HU_PCT != "NA" & HU_PCT >= 1 & size_class != "Medium") %>%
  mutate(buckets = bucket(HU_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(size_class = factor(size_class, levels=c("Small", "Large"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ size_class, switch = "y")


colourCount = length(unique(bucket(EcoSzCl_Df$LG_PCT, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))
p2.1 <- EcoSzCl_Df %>%
  filter(LG_PCT != "NA" & LG_PCT >= 1 & size_class != "Medium") %>%
  mutate(buckets = bucket(LG_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(size_class = factor(size_class, levels=c("Small", "Large"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ size_class, switch = "y")


p3 <- EcoSeason_Df %>%
  filter(HU_PCT != "NA" & HU_PCT >= 1) %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  mutate(buckets = bucket(HU_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Spring","Summer"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .01) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ seasons, switch = "y")

colourCount = length(unique(bucket(EcoSeason_Df$LG_PCT, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))

p3.1 <- EcoSeason_Df %>%
  filter(LG_PCT != "NA" & LG_PCT >= 1) %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  mutate(buckets = bucket(LG_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter","Spring","Summer", "Fall"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .01) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ seasons, switch = "y")

ggsave(file = "Ecoreg_FireCount_Hum.png", p1.1, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Ecoreg_FireCount_Lig.png", p1.2, width = 5, height = 7, dpi=1000) #saves g

ggsave(file = "Ecoreg_PctHum_SizeCl_Class.png", p2,  width = 8, height = 5, dpi=1000) #saves g
ggsave(file = "Ecoreg_PctLght_SizeCl_Class.png", p2.1,  width = 8, height = 5, dpi=1000) #saves g

ggsave(file = "Ecoreg_PctHum_Season_Class.png", p3, width = 8, height = 5, dpi=1000) #saves g
ggsave(file = "Ecoreg_PctLig_Season_Class.png", p3.1, width = 8, height = 5, dpi=1000) #saves g




# Area observed IGNITIONs proportional to the total class area ***Short**** --------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion")

wuw_eco_poly <- wuw_short %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  

eco_sum <- wuw_eco_poly %>%
  group_by(US_L3NAME) %>%
  summarize(sum_totfire = sum(fire_size_km2),
            tot_fire = n()) %>%
  ungroup() %>%  filter(US_L3NAME != "")

eco_sum2 <- wuw_eco_poly %>%
  group_by(US_L3NAME, Class) %>%
  summarize(sum_totfire = sum(fire_size_km2),
            tot_fire = n()) %>%
  ungroup() %>% 
  filter(US_L3NAME != "")

Eco_Fden <- wuw_eco_poly %>%
  group_by(US_L3NAME, VLD, Wildlands, WUI, IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  filter(VLD != "NA") %>%
  spread(IGNITION, n_fire) %>%
  left_join(., eco_sum2, by = c("Class", "US_L3NAME")) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human), 
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         human_den = ifelse(Class == "VLD", ((tot_fire-Human)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Human)/(WUI)),
                                   ((tot_fire-Human)/(Wildlands)))),
         light_den = ifelse(Class == "VLD", ((tot_fire-Lightning)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Lightning)/(WUI)),
                                   ((tot_fire-Lightning)/(Wildlands)))),
         f_den = (1-(human_den/(human_den+light_den)))*100,
         us_l3name = US_L3NAME)

eco_den_shp <- left_join(er_df, Eco_Fden, by = "us_l3name")

Eco_Pct <- wuw_eco_poly %>%
  group_by(US_L3NAME, EcReg_Area_km2.x, IGNITION, Class) %>%
  summarize(sum_fire = sum(fire_size_km2)) %>%
  ungroup() %>%
  filter(EcReg_Area_km2.x != "NA") %>%
  left_join(., eco_sum, by = "US_L3NAME") %>%
  spread(IGNITION, sum_fire) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human), 
         Lightning = ifelse(is.na(Lightning), 0, Lightning), 
         Human_Norm_EcoR = (Human/sum_totfire),
         Lightning_Norm_EcoR = (Lightning/sum_totfire),
         Human_Pct_Norm = Human_Norm_EcoR/(Human_Norm_EcoR+Lightning_Norm_EcoR)*100,
         Lightning_Pct_Norm = Human_Pct_Norm - 100,
         HU_PCT = ((Human/sum_totfire)*100),
         LG_PCT = ((Lightning/sum_totfire)*100),
         us_l3name = US_L3NAME)

Eco_Df2 <- left_join(er_df, Eco_Pct, by = "us_l3name")

wuw_eco_poly_hum <- wuw_short %>%
  filter(IGNITION != "Lighting") %>%
  #distinct(BLK10, .keep_all = TRUE) %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  

wuw_eco_poly_lig <- wuw_short %>%
  filter(IGNITION != "Human") %>%
  #distinct(BLK10, .keep_all = TRUE) %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  

EcoPolyPropIgnition_PctHum <- wuw_eco_poly_hum %>%
  filter(EcReg_Area_km2.y != "NA") %>%
  group_by(US_L3NAME, IGNITION, Class, EcReg_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(fire_size_km2)) %>%
  ungroup() %>%
  spread(IGNITION, sum_cb) %>%
  mutate(human_pct = ifelse(Class == "VLD", (Human/(VLD))*100, 
                            ifelse(Class == "WUI", (Human/(WUI))*100,
                                   (Human/(Wildlands))*100)),
         IGNITION = "Human") %>%
  select(US_L3NAME, Class, EcReg_Area_km2.y, Wildlands, WUI, VLD, Human, human_pct)

EcoPolyPropIgnition_PctLig <- wuw_eco_poly_lig %>%
  filter(EcReg_Area_km2.y != "NA") %>%
  group_by(US_L3NAME, IGNITION, Class, EcReg_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(fire_size_km2)) %>%
  ungroup() %>%
  spread(IGNITION, sum_cb) %>%
  mutate(light_pct = ifelse(Class == "VLD", (Lightning/(VLD))*100, 
                            ifelse(Class == "WUI", (Lightning/(WUI))*100,
                                   (Lightning/(Wildlands))*100)),
         Ignition = "Lightning") %>%
  select(US_L3NAME, Class, Lightning, light_pct)

Eco_poly <- full_join(EcoPolyPropIgnition_PctHum, EcoPolyPropIgnition_PctLig, by = c("US_L3NAME", "Class")) %>%
  select(US_L3NAME, Class, EcReg_Area_km2.y , Human, Lightning, human_pct, light_pct) %>%
  ungroup() %>%
  mutate(PctEco_Hum = Human/EcReg_Area_km2.y *100,
         PctEco_Lig = Lightning/EcReg_Area_km2.y *100,
         PctAreaHum = ifelse(is.na(human_pct), 0, human_pct),
         PctAreaLig = ifelse(is.na(light_pct), 0, light_pct),
         PctArea = PctAreaHum + PctAreaLig,
         us_l3name = US_L3NAME)

eco_poly_shp <- left_join(er_df, Eco_poly, by = "us_l3name")

colourCount2 = length(unique(bucket(eco_den_shp$f_den, 10)))
getPalette2 = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")

p100 <- eco_den_shp %>%
  filter(f_den != "NA") %>%
  filter(f_den >= 1) %>%
  mutate(buckets = bucket(f_den, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16),
        #,legend.position="none"
        ) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')
legend <- g_legend(p100)
ggsave(file = "Gradient_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g

colourCount = length(unique(bucket(Eco_Df2$Human_Pct_Norm, 10)))
getPalette2 = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")

p1 <- Eco_Df2 %>%
  filter(Human_Pct_Norm != "NA") %>%
  filter(Human_Pct_Norm >= 1) %>%
  mutate(buckets = bucket(Human_Pct_Norm, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  ggtitle('(B) Percent burn area') +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')
legend <- g_legend(p1)
ggsave(file = "Gradient_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g

grid.arrange(p100, p1, ncol = 2)
g <- arrangeGrob(p100, p1, ncol = 2) #generates g
ggsave(file = "Eco_PctFDen_PctBArea.png", g, width = 10, height = 6, dpi=1200) #saves g


p5.12 <- eco_poly_shp %>%
  filter(PctEco_Hum != "NA") %>%
  filter(PctEco_Hum >= 1) %>%
  mutate(buckets = bucket(PctEco_Hum, 2)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.13 <- eco_poly_shp %>%
  filter(PctEco_Lig != "NA") %>%
  filter(PctEco_Lig >= 1) %>%
  mutate(buckets = bucket(PctEco_Lig, 2)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

grid.arrange(p5.12, p5.13, ncol= 2)

p5.1 <- eco_poly_shp %>%
  filter(PctEco_Hum != "NA") %>%
  filter(PctEco_Hum >= 1) %>%
  mutate(buckets = bucket(PctEco_Hum, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.2 <- eco_poly_shp %>%
  filter(PctEco_Lig != "NA") %>%
  filter(PctEco_Lig >= 1) %>%
  mutate(buckets = bucket(PctEco_Lig, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')
ggsave(file = "Ecoreg_PctCBBurn_Human.png", p5.1, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Ecoreg_PctCBBurn_Lightning.png", p5.2, width = 5, height = 7, dpi=1000) #saves g

#Seasonal 
wuw_eco_poly <- wuw_short %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10),
         Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  

eco_sum <- wuw_eco_poly %>%
  group_by(US_L3NAME, Seasons) %>%
  summarize(sum_totfire = sum(fire_size_km2),
            tot_fire = n()) %>%
  ungroup() %>%  filter(US_L3NAME != "")

eco_sum2 <- wuw_eco_poly %>%
  group_by(US_L3NAME, Class, Seasons) %>%
  summarize(sum_totfire = sum(fire_size_km2),
            tot_fire = n()) %>%
  ungroup() %>% 
  filter(US_L3NAME != "")

Eco_Fden <- wuw_eco_poly %>%
  group_by(US_L3NAME, VLD, Wildlands, WUI, IGNITION, Class, Seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  filter(VLD != "NA") %>%
  spread(IGNITION, n_fire) %>%
  left_join(., eco_sum2, by = c("Class", "US_L3NAME", "Seasons")) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human), 
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         human_den = ifelse(Class == "VLD", ((tot_fire-Human)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Human)/(WUI)),
                                   ((tot_fire-Human)/(Wildlands)))),
         light_den = ifelse(Class == "VLD", ((tot_fire-Lightning)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Lightning)/(WUI)),
                                   ((tot_fire-Lightning)/(Wildlands)))),
         f_den = (1-(human_den/(human_den+light_den)))*100,
         us_l3name = US_L3NAME)

eco_den_shp <- left_join(er_df, Eco_Fden, by = "us_l3name")

Eco_Pct <- wuw_eco_poly %>%
  group_by(US_L3NAME, EcReg_Area_km2.x, IGNITION, Class, Seasons) %>%
  summarize(sum_fire = sum(fire_size_km2)) %>%
  ungroup() %>%
  filter(EcReg_Area_km2.x != "NA") %>%
  left_join(., eco_sum, by = c("US_L3NAME", "Seasons")) %>%
  spread(IGNITION, sum_fire) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human), 
         Lightning = ifelse(is.na(Lightning), 0, Lightning), 
         Human_Norm_EcoR = (Human/sum_totfire),
         Lightning_Norm_EcoR = (Lightning/sum_totfire),
         Human_Pct_Norm = Human_Norm_EcoR/(Human_Norm_EcoR+Lightning_Norm_EcoR)*100,
         Lightning_Pct_Norm = Human_Pct_Norm - 100,
         HU_PCT = ((Human/sum_totfire)*100),
         LG_PCT = ((Lightning/sum_totfire)*100),
         us_l3name = US_L3NAME)

Eco_Df2 <- left_join(er_df, Eco_Pct, by = "us_l3name")

colourCount2 = length(unique(bucket(eco_den_shp$f_den, 10)))
getPalette2 = colorRampPalette(c("blue", "white","red"),
                               space = "Lab")

p100 <- eco_den_shp %>%
  filter(f_den != "NA") %>%
  filter(f_den >= 1) %>%
  filter(Class == "WUI" | Class == "Wildlands") %>%
  filter(Seasons == "Spring" | Seasons == "Summer") %>%
  mutate(buckets = bucket(f_den, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Percent fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16),
        legend.position="none") +
  facet_grid(Class~ Seasons)

colourCount2 = length(unique(bucket(Eco_Df2$Human_Pct_Norm, 10)))
getPalette2 = colorRampPalette(c("blue", "white","red"),
                              space = "Lab")

p1 <- Eco_Df2 %>%
  filter(Human_Pct_Norm != "NA") %>%
  filter(Human_Pct_Norm >= 1) %>%
  filter(Class == "WUI" | Class == "Wildlands") %>%
  filter(Seasons == "Spring" | Seasons == "Summer") %>%
  mutate(buckets = bucket(Human_Pct_Norm, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  ggtitle('(B) Percent burn area') +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16),
        legend.position="none") +
  facet_grid(Class~ Seasons)

g <- arrangeGrob(p100, p1, ncol = 1) #generates g
ggsave(file = "Eco_SpringSummer_PctBArea.png", g, width = 6, height = 10, dpi=1200) #saves g



# Time Series East, West, Central -----------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion")

levels(wuw_short$Region)[levels(wuw_short$Region)=="North East"] <- "East"
levels(wuw_short$Region)[levels(wuw_short$Region)=="South East"] <- "East"

fit_mblm <- function(df) {
  # estimate intercept and slope
  # input: 
  #  - df (data.frame): has columns FIRE_YEAR & N
  # return:
  #  - data.frame with intercept and slope
  #y <- df[[y]]
  y <- df$N
  x <- df$FIRE_YEAR
  m <- mblm(y ~ x, repeated = FALSE)
  data.frame(intercept = m$coefficients[1], 
             slope = m$coefficients[2])
}

estimates <- wuw_short %>%
  filter(Region != "") %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  %>%
  group_by(FIRE_YEAR, Region, Class, IGNITION, fire_size_km2) %>%
  summarize(N = n(),
            TotBurn = sum(fire_size_km2)) %>%
  ungroup() %>% 
  group_by(Class, IGNITION) %>%
  do(fit_mblm(.))

wuw_short %>%
  filter(Region != "") %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  %>%
  group_by(FIRE_YEAR, Region, Class, IGNITION, fire_size_km2) %>%
  summarize(N = n(),
            TotBurn = sum(fire_size_km2)) %>%
  ungroup() %>%
  ggplot(aes(x = FIRE_YEAR, y = N, color = size_class)) + 
  geom_point() +theme_bw() +
  facet_wrap(Class ~ Ignition, scales = "free") + 
  geom_abline(aes(slope = slope, intercept = intercept, color = size_class), 
            data = estimates)



t2 <- wuw_eco_poly %>%
  filter( Class == "WUI") %>%
  ggplot(aes(x = as.factor(FIRE_YEAR), y = fire_size_km2, fill = IGNITION)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  #scale_fill_manual(values=c("light gray", "gray40")) +
  #scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("Ignition Type") + ylab("Number of fire ignitions") +
  facet_grid( ~ Region) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


# MTBS Burned Area Denisty ------------------------------------------------

setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion")

wuw_eco_poly <- bae %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")  

eco_sum <- wuw_eco_poly %>%
  group_by(US_L3NAME) %>%
  summarize(sum_totfire = sum(AREA_km2),
            tot_fire = n()) %>%
  ungroup() %>%  filter(US_L3NAME != "")

eco_sum2 <- wuw_eco_poly %>%
  group_by(US_L3NAME, Class) %>%
  summarize(sum_totfire = sum(AREA_km2),
            tot_fire = n()) %>%
  ungroup() %>% 
  filter(US_L3NAME != "")

Eco_Fden <- wuw_eco_poly %>%
  group_by(US_L3NAME, VLD, Wildlands, WUI, IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  filter(VLD != "NA") %>%
  spread(IGNITION, n_fire) %>%
  left_join(., eco_sum2, by = c("Class", "US_L3NAME")) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human), 
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         human_den = ifelse(Class == "VLD", ((tot_fire-Human)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Human)/(WUI)),
                                   ((tot_fire-Human)/(Wildlands)))),
         light_den = ifelse(Class == "VLD", ((tot_fire-Lightning)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Lightning)/(WUI)),
                                   ((tot_fire-Lightning)/(Wildlands)))),
         f_den = (1-(human_den/(human_den+light_den)))*100,
         us_l3name = US_L3NAME)

eco_den_shp <- left_join(er_df, Eco_Fden, by = "us_l3name")

colourCount2 = length(unique(bucket(eco_den_shp$f_den, 10)))
getPalette2 = colorRampPalette(c("royalblue2", "peachpuff2","red2"),
                               space = "Lab")

p100 <- eco_den_shp %>%
  filter(f_den != "NA") %>%
  filter(f_den >= 1) %>%
  filter(Class == "WUI" | Class == "Wildlands") %>%
  mutate(buckets = bucket(f_den, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Percent fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(~ Class)

# Area observed igntions proportional to the total class area ****bae -  Human and lightning**** -------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion")

wuw_eco_polyhum <- bae %>%
  filter(IGNITION == "Human") %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  #distinct(., BLK10, .keep_all = TRUE) %>%
  left_join(., area_grp_eco, by = "US_L3NAME") %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

eco_ign_freq <- wuw_eco_polyhum %>%
  group_by(US_L3NAME) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

eco_poly_hum <- wuw_eco_polyhum %>%
  group_by(US_L3NAME, Class, EcReg_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(AREA_km2)) %>%
  ungroup() %>%
  left_join(., eco_ign_freq, by = "US_L3NAME") %>%
  mutate(Human = ifelse(is.na(sum_cb), 0 ,sum_cb),
         human_pct = ifelse(Class == "VLD", (Human/(VLD))*100, 
                            ifelse(Class == "WUI", (Human/(WUI))*100,
                                   (Human/(Wildlands))*100)),
         PctEco_Hum = Human/EcReg_Area_km2.y *100,
         PctAreaHum = ifelse(is.na(human_pct), 0, human_pct),
         us_l3name = US_L3NAME) %>%
  filter(VLD != "NA") 

wuw_eco_polylig <- bae %>%
  filter(IGNITION == "Lightning") %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  #distinct(., BLK10, .keep_all = TRUE) %>%
  left_join(., area_grp_eco, by = "US_L3NAME") %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")  

eco_ign_freq <- wuw_eco_polylig %>%
  group_by(US_L3NAME) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

eco_poly_lig <- wuw_eco_polylig %>%
  group_by(US_L3NAME, Class, EcReg_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(AREA_km2)) %>%
  ungroup() %>%
  left_join(., eco_ign_freq, by = "US_L3NAME") %>%
  mutate(Lightning = ifelse(is.na(sum_cb), 0 ,sum_cb),
         light_pct = ifelse(Class == "VLD", (Lightning/(VLD)*100), 
                            ifelse(Class == "WUI", (Lightning/(WUI)*100),
                                   (Lightning/(Wildlands)*100))),
         PctEco_Lig = Lightning/EcReg_Area_km2.y *100,
         PctAreaLig = ifelse(is.na(light_pct), 0, light_pct),
         us_l3name = US_L3NAME) %>%
  filter(VLD != "NA")

eco_poly_shp_hum <- left_join(er_df, eco_poly_hum, by = "us_l3name")
eco_poly_shp_lig <- left_join(er_df, eco_poly_lig, by = "us_l3name")

p4.9 <- eco_poly_shp_hum %>%
  filter(PctEco_Hum != "NA") %>%
  filter(PctEco_Hum >= 1) %>%
  mutate(buckets = bucket(PctEco_Hum, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.0 <- eco_poly_shp_lig %>%
  filter(PctEco_Lig != "NA") %>%
  filter(PctEco_Lig >= 1) %>%
  mutate(buckets = bucket(PctEco_Lig, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.1 <- eco_poly_shp_hum %>%
  filter(PctAreaHum != "NA") %>%
  filter(PctAreaHum >= 1) %>%
  mutate(buckets = bucket(PctAreaHum, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  #scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.2 <- eco_poly_shp_lig %>%
  filter(PctAreaLig != "NA") %>%
  filter(PctAreaLig >= 1) %>%
  mutate(buckets = bucket(PctAreaLig, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  #scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

colourCount = length(unique(bucket(eco_poly_shp_hum$sum_cb, 1000)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
p5.3 <- eco_poly_shp_hum %>%
  filter(Human >= 1) %>%
  mutate(buckets = bucket(Human, 1000)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

colourCount2 = length(unique(bucket(eco_poly_shp_lig$Lightning, 4000)))
getPalette2 = colorRampPalette(brewer.pal(9, "YlGnBu"))
p5.4 <- eco_poly_shp_lig %>%
  filter(Lightning >= 1) %>%
  mutate(buckets = bucket(Lightning,4000)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

ggsave(file = "Ecoreg_PctEcoBurn_Human_MTBS.png", p4.9, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Ecoreg_PctEcoBurn_Lightning_MTBS.png", p5.0, width = 5, height = 7, dpi=1000) #saves g

ggsave(file = "Ecoreg_PctClassBurn_Human_MTBS.png", p5.1, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Ecoreg_PctClassBurn_Lightning_MTBS.png", p5.2, width = 5, height = 7, dpi=1000) #saves g

ggsave(file = "Ecoreg_TotBurnArea_Human_MTBS.png", p5.3, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Ecoreg_TotBurnArea_Lig_MTBS.png", p5.4, width = 5, height = 7, dpi=1000) #saves g


# # MTBS  ----------------------------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion")

wuw_mtbs_slim <- bae %>%
  #distinct(., BLK10, .keep_all = TRUE) %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_eco, by = "US_L3NAME")

EcoPolyM <- wuw_mtbs_slim %>%
  group_by(US_L3NAME, Class, EcReg_Area_km2.x, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(AREA_km2)) %>%
  ungroup() %>%
  mutate(burn_pct_class = ifelse(Class == "VLD", (sum_cb/(VLD))*100, 
                                 ifelse(Class == "WUI", (sum_cb/(WUI))*100,
                                        (sum_cb/(Wildlands))*100)),
         burn_pct_eco = (sum_cb/(EcReg_Area_km2.x))*100,
         us_l3name = US_L3NAME)

baecv_slim <- baecv[!duplicated(baecv$BLK10),]
 baecv_slim <- baecv %>%#
   select(BLK10, US_L3NAME, AREA, WUICLASS10, Year, Area_km2) %>%
   mutate(DataType = "BAECV") %>%
   mutate(AREA = as.numeric(AREA),
        Class = classify_new_categories(WUICLASS10)) %>%
   filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
   left_join(., econorm, by = "US_L3NAME")
baecv_merge <- wuw_mtbs_slim %>%
   merge(., baecv_mtbs, all= TRUE)

EcoPolyB <- baecv_slim %>%
  group_by(US_L3NAME, Class, EcReg_Area_km2.x, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(AREA_km2)) %>%
  ungroup() %>%
  mutate(burn_pct_class = ifelse(Class == "VLD", (sum_cb/(VLD))*100, 
                                 ifelse(Class == "WUI", (sum_cb/(WUI))*100,
                                        (sum_cb/(Wildlands))*100)),
         burn_pct_eco = (sum_cb/(EcReg_Area_km2.x))*100,
         burn_pct_class = ifelse(burn_pct_class >100 , 100, burn_pct_class),
         burn_pct_eco = ifelse(burn_pct_eco >100 , 100, burn_pct_eco),
         us_l3name = US_L3NAME)

EcoPolyBM <- baecv_merge %>%
  group_by(US_L3NAME, Class, USL3_km2, Wild_km2, WUI_km2, VLD_km2) %>%
  summarize(sum_cb = sum(AREA)) %>%
  ungroup() %>%
  mutate(burn_pct_class = ifelse(Class == "VLD", (sum_cb/(VLD_km2))*100, 
                                 ifelse(Class == "WUI", (sum_cb/(WUI_km2))*100,
                                        (sum_cb/(Wild_km2))*100)),
         burn_pct_eco = (sum_cb/(USL3_km2))*100,
         burn_pct_class = ifelse(burn_pct_class >100 , 100, burn_pct_class),
         burn_pct_eco = ifelse(burn_pct_eco >100 , 100, burn_pct_eco),
         us_l3name = US_L3NAME)

eco_poly_m <- left_join(er_df, EcoPolyM, by = "us_l3name") 
eco_poly_b <- left_join(er_df, EcoPolyB, by = "us_l3name") 
eco_poly_bm <- left_join(er_df, EcoPolyBM, by = "us_l3name") 

colourCount = length(unique(bucket(eco_poly_m$burn_pct_class, 5)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
p4.91 <- eco_poly_m %>%
  filter(burn_pct_class != "NA") %>%
  #filter(burn_pct_class >= 1) %>%
  mutate(buckets = bucket(burn_pct_class, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

#Only BAECV burned area 
colourCount = length(unique(bucket(eco_poly_b$burn_pct_class, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
p4.91 <- eco_poly_b %>%
  filter(burn_pct_class != "NA") %>%
  filter(burn_pct_class >= 1) %>%
  mutate(buckets = bucket(burn_pct_class, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')


#Both BAECV and MTBS burned area 
colourCount = length(unique(bucket(eco_poly_bm$burn_pct_class, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
p4.9 <- eco_poly_bm %>%
  filter(burn_pct_class != "NA") %>%
  filter(burn_pct_class >= 1) %>%
  mutate(buckets = bucket(burn_pct_class, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.0 <- eco_poly_bm %>%
  filter(burn_pct_eco != "NA") %>%
  filter(burn_pct_eco >= 1) %>%
  mutate(buckets = bucket(burn_pct_eco, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

ggsave(file = "Ecoreg_PctClassBurn_BAECV.png", p4.91, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Ecoreg_PctClassBurn_BAECV_MTBS.png", p4.9, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Ecoreg_PctEcoBurn_BAECV.png", p5.0, width = 5, height = 7, dpi=1000) #saves g

# # ICS 209 and ecoregion assessment ----------------------------------------

ICS_red <- ICS[!duplicated(ICS$IncidentNu),]

wuw_ics209_cln <- ICS_red %>%
  mutate(costs = as.numeric(SuppressCo),
         Class = classify_new_categories(WUICLASS10)) %>%
  ungroup()

ICS_short <- wuw_short %>%
  select(ICS_209_INCIDENT_NUMBER, IGNITION, US_L3NAME) %>%
  right_join(., wuw_ics209_cln, by = c("ICS_209_INCIDENT_NUMBER" = "IncidentNu"))

ICS_short <- ICS_short[!is.na(ICS_short$IGNITION),]

eco_Cost_209 <- ICS_short %>%
  group_by(US_L3NAME) %>%
  summarise(tot_cost = (sum(SuppressCo))) %>%
  ungroup()

eco_l1 <- ICS_short %>%
  group_by(US_L3NAME, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(SuppressCo))) %>%
  left_join(., eco_Cost_209, by = c("US_L3NAME")) %>%
  mutate(ignclass_tots = ifelse(is.na(ignclass_tots), 0, ignclass_tots),
         tot_cost = ifelse(is.na(tot_cost), 0, tot_cost),
         human_pct_cost = ifelse(is.na((ignclass_tots/(tot_cost)*100)), 0,
                                 round((ignclass_tots/(tot_cost)*100), digits =1)),
         us_l3name = US_L3NAME,
         ICS_Report = "Total Coeco")%>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

eco_l2 <- ICS_short%>%
  group_by(US_L3NAME, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(SuppressCo))) %>%
  left_join(., eco_Cost_209, by = c("US_L3NAME")) %>%
  spread(IGNITION, ignclass_tots) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human),
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Pct_Combine = (Human/(Human+Lightning))*100,
         tot_cost = ifelse(is.na(tot_cost), 0, tot_cost),
         us_l3name = US_L3NAME,
         ICS_Report = "Total Coeco") %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

eco_l3 <- ICS_short%>%
  group_by(US_L3NAME, IGNITION) %>%
  summarise(ignclass_tots = (sum(SuppressCo))) %>%
  left_join(., eco_Cost_209, by = c("US_L3NAME")) %>%
  mutate(ignclass_tots = ifelse(is.na(ignclass_tots), 0, ignclass_tots),
         tot_cost = ifelse(is.na(tot_cost), 0, tot_cost),
         human_pct_cost = ifelse(is.na((ignclass_tots/(tot_cost)*100)), 0,
                                 round((ignclass_tots/(tot_cost)*100), digits =1)),
         us_l3name = US_L3NAME,
         ICS_Report = "Total Coeco")%>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

eco_df_l1 <- left_join(er_df, eco_l1, by = "us_l3name")
eco_df_l1 <- na.omit(eco_df_l1)

eco_df_l2 <- left_join(er_df, eco_l2, by = "us_l3name")
eco_df_l2 <- na.omit(eco_df_l2)

eco_df_l3 <- left_join(er_df, eco_l3, by = "us_l3name")

setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion/ICS209")

colourCount8 = length(unique(bucket(eco_df_l1$human_pct_cost, 10)))
getPalette8 = colorRampPalette(brewer.pal(9, "YlOrRd"))

p6 <- eco_df_l1 %>%
  filter(human_pct_cost != "NA") %>%
  filter(human_pct_cost >= 1 & IGNITION != "Lightning") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette8(colourCount8), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class ~ IGNITION, switch="y")

colourCount2 = length(unique(bucket(eco_df_l1$human_pct_cost, 10)))
getPalette2 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p65 <- eco_df_l1 %>%
  filter(human_pct_cost >= 1 & IGNITION != "Human") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class ~ IGNITION, switch="y")

colourCount5 = length(unique(bucket(eco_df_l3$human_pct_cost, 10)))
getPalette5 = colorRampPalette(brewer.pal(9, "YlOrRd"))

p64 <- eco_df_l3 %>%
  filter(human_pct_cost != "NA" & IGNITION != "Lightning") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette5(colourCount5), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

colourCount4 = length(unique(bucket(eco_df_l3$human_pct_cost, 10)))
getPalette4 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p63 <- eco_df_l3 %>%
  filter(human_pct_cost != "NA"  & IGNITION != "Human") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette4(colourCount4), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

colourCount = length(unique(bucket(eco_df_l2$Pct_Combine, 10)))
getPalette = colorRampPalette(brewer.pal(9, "RdYlBu"))

p62 <- eco_df_l2 %>%
  filter(Pct_Combine != "NA") %>%
  filter(Pct_Combine >= 1) %>%
  mutate(buckets = bucket(Pct_Combine, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol = 1, strip.position = 'left')

grid.arrange(p6, p65, ncol = 2)
g <- arrangeGrob(p6, p65, ncol = 2) #generates g
ggsave(file = "PctCoeco_EcoR_Class.png", g, width = 10, height = 6, dpi=1200) #saves g

grid.arrange(p64, p63, ncol = 1)
g <- arrangeGrob(p64, p63, ncol = 1) #generates g
ggsave(file = "PctCosts_EcoR_NoClass.png", g, width = 5, height = 7, dpi=1200) #saves g

g <- arrangeGrob(p62, ncol = ) #generates g
ggsave(file = "PctCostsCbm_EcoR_pClass.png", g, width = 5, height = 7, dpi=1200) #saves g

# Structures --------------------------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion/ICS209")

ICS_red <- ICS[!duplicated(ICS$IncidentNu),]

wuw_ics209_cln <- ICS_red %>%
  mutate(resd = as.numeric(ResDestroy),
         Class = classify_new_categories(WUICLASS10)) %>%
  ungroup()

ICS_short <- wuw_short %>%
  select(ICS_209_INCIDENT_NUMBER, IGNITION, US_L3NAME) %>%
  right_join(., wuw_ics209_cln, by = c("ICS_209_INCIDENT_NUMBER" = "IncidentNu"))

ICS_short <- ICS_short[!is.na(ICS_short$IGNITION),]

eco_stru_209 <- ICS_short %>%
  group_by(US_L3NAME) %>%
  summarise(tot_struc = (sum(ResDestroy))) %>%
  ungroup()

eco_str_l1 <- ICS_short %>%
  group_by(US_L3NAME, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(ResDestroy))) %>%
  left_join(., eco_stru_209, by = c("US_L3NAME")) %>%
  mutate(ignclass_tots = ifelse(is.na(ignclass_tots), 0, ignclass_tots),
         tot_struc = ifelse(is.na(tot_struc), 0, tot_struc),
         human_pct_cost = ifelse(is.na((ignclass_tots/(tot_struc)*100)), 0,
                                 round((ignclass_tots/(tot_struc)*100), digits =1)),
         us_l3name = US_L3NAME,
         ICS_Report = "Total Coeco")%>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

eco_str_l2 <- ICS_short%>%
  group_by(US_L3NAME, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(ResDestroy))) %>%
  left_join(., eco_stru_209, by = c("US_L3NAME")) %>%
  spread(IGNITION, ignclass_tots) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human),
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Pct_Combine = ifelse(is.na((Human/(Human+Lightning))*100), 0, (Human/(Human+Lightning))*100),
         tot_struc = ifelse(is.na(tot_struc), 0, tot_struc),
         us_l3name = US_L3NAME,
         ICS_Report = "Total Coeco")

eco_str_df_l1 <- left_join(er_df, eco_str_l1, by = "us_l3name")
eco_str_df_l2 <- left_join(er_df, eco_str_l2, by = "us_l3name")


colourCount = length(unique(bucket(eco_str_df_l1$human_pct_cost, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

p621 <- eco_str_df_l1 %>%
  filter(human_pct_cost >= 1 & IGNITION != "Lightning") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol = 1, strip.position = 'left')

colourCount2 = length(unique(bucket(eco_str_df_l1$human_pct_cost, 10)))
getPalette2 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p622 <- eco_str_df_l1 %>%
  filter(human_pct_cost >= 1 & IGNITION != "Human") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol = 1, strip.position = 'left')

colourCount5 = length(unique(bucket(eco_str_df_l1$human_pct_cost, 10)))
getPalette5 = colorRampPalette(brewer.pal(9, "YlOrRd"))

p64 <- eco_str_df_l1 %>%
  filter(human_pct_cost != "NA" & IGNITION != "Lightning") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette5(colourCount5), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

colourCount4 = length(unique(bucket(eco_str_df_l1$human_pct_cost, 10)))
getPalette4 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p63 <- eco_str_df_l1 %>%
  filter(human_pct_cost != "NA"  & IGNITION != "Human") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette4(colourCount4), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

grid.arrange(p621, p622, ncol = 2)
g <- arrangeGrob(p621, p622, ncol = 2) #generates g
ggsave(file = "PctStruct_EcoR_Class.png", g, width = 10, height = 6, dpi=1200) #saves g

grid.arrange(p64, p63, ncol = 1)
g <- arrangeGrob(p64, p63, ncol = 1) #generates g
ggsave(file = "PctStruct_EcoR_NoClass.png", g, width = 5, height = 7, dpi=1200) #saves g


# Ecoregion - the max seasonality of fire occurence  ----------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Ecoregion")

wuw_seasonality_prep <- wuw_short %>%
  filter(Class != "Urban")%>%
  mutate(fire_size_km2 = (FIRE_SIZE * 0.00404686),
         size_class = classify_fire_size(fire_size_km2),
         seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10)) %>%
  left_join(., econorm, by = "US_L3NAME")  

wuw_short_seasonality <- wuw_seasonality_prep %>%
  group_by(US_L3NAME, Class, IGNITION, seasons) %>%
  summarize(n_count = n()) %>%
  ungroup %>%
  spread(seasons, n_count) %>%
  group_by(US_L3NAME, Class, IGNITION) %>%
  do(get_month_max(.)) %>%
  mutate(us_l3name = US_L3NAME)

eco_seasonality_shp <- left_join(er_df, wuw_short_seasonality, by = "us_l3name")


p25 <- eco_seasonality_shp %>%
  filter(Class != "NA")%>%
  #mutate(buckets = bucket(PctBurn, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = max_season)) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(position ="left", name="Seasons", direction =-1 , palette = "BrBG", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=14)) +
  facet_grid(Class ~ IGNITION, switch="y")
ggsave(file = "Ecoreg_MaxSeasonality_Class.png", p25, width = 8, height = 5, dpi=1200) #saves g









# Normalized by State ------------------------------------------------------
# Summarize States information using dplyr ---------------------------------------------------
wuw_sts<- wuw_short %>%
  filter(Class != "Urban")%>%
  mutate(fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         size_class = classify_fire_size(fire_size_km2),
         seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10)) %>%
  left_join(., stnorm, by = "STUSPS")  

sts_freq <- wuw_sts %>%
  group_by(STUSPS, IGNITION) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

sts_Cnt <- wuw_sts %>%
  group_by(STUSPS, IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., sts_freq, by = c("STUSPS", "IGNITION")) %>%
  mutate( stusps = STUSPS,
          fire_freq = n_fire*0.001,
          fire_freq = ifelse(fire_freq == 0, 0, fire_freq),
          tot_pct_freq = (n_fire/tot_fire)*100)
stsCnt_Df <- left_join(st_df, sts_Cnt, by = "stusps")

sts_ign_freq <- wuw_sts %>%
  group_by(STUSPS) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

sts_Pct <- wuw_sts %>%
  group_by(STUSPS, State_Km2.y,IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., sts_ign_freq, by = "STUSPS") %>%
  spread(IGNITION, n_fire) %>%
  mutate(Lightning = ifelse(is.na(Lightning), 0, Lightning), 
         Human_Norm_stsR = (Human/tot_fire),
         Lightning_Norm_stsR = (Lightning/tot_fire),
         Human_Pct_Norm = Human_Norm_stsR/(Human_Norm_stsR+Lightning_Norm_stsR)*100,
         Lightning_Pct_Norm = Human_Pct_Norm - 100,
         HU_PCT = ((Human/tot_fire)*100),
         LG_PCT = ((Lightning/tot_fire)*100),
         stusps = STUSPS)


sts_Df <- left_join(st_df, sts_Pct, by = "stusps")

SzSts_freq <- wuw_sts %>%
  filter(size_class != "Medium") %>%
  group_by(STUSPS, IGNITION) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

stsSzCl_Pct <- wuw_sts %>%
  group_by(STUSPS, IGNITION, Class, size_class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., SzSts_freq, by = c("STUSPS", "IGNITION")) %>%
  spread(IGNITION, n_fire) %>%
  mutate(Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Human = ifelse(is.na(Human), 0, Human),
         HU_PCT = ((Human/tot_fire)*100),
         LG_PCT = ((Lightning/tot_fire)*100),
         stusps = STUSPS)

stsSzCl_Df <- left_join(st_df, stsSzCl_Pct, by = "stusps")
stsSzCl_Df <- na.omit(stsSzCl_Df)


SeasonSts_freq <- wuw_sts %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  group_by(STUSPS, IGNITION) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 


stsSeason_Pct <- wuw_sts %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  group_by(STUSPS, IGNITION, Class, seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  left_join(., SeasonSts_freq, by = c("STUSPS", "IGNITION")) %>%
  spread(IGNITION, n_fire) %>%
  mutate(Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Human = ifelse(is.na(Human), 0, Human),
         HU_PCT = ((Human/tot_fire)*100),
         LG_PCT = ((Lightning/tot_fire)*100),
         stusps = STUSPS)

stsSeason_Df <- left_join(st_df, stsSeason_Pct, by = "stusps")
stsSeason_Df <- na.omit(stsSeason_Df)


# Plotting States ----------------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/States")

p1.0 <-  stsCnt_Df %>%
  filter(tot_pct_freq != "NA") %>%
  filter(tot_pct_freq >= 1) %>%
  mutate(buckets = bucket(tot_pct_freq, 25)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1) +
  coord_equal() + 
  scale_fill_brewer(name="Num of Fire\n (in thousands)", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ IGNITION, switch = "y")

p1.01 <-  stsCnt_Df %>%
  filter(tot_pct_freq != "NA") %>%
  filter(tot_pct_freq >= 1) %>%
  mutate(buckets = bucket(tot_pct_freq, 25)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1) +
  coord_equal() + 
  scale_fill_brewer(name="Num of Fire\n (in thousands)", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ IGNITION, switch = "y")

p1 <- sts_Df %>%
  filter(Human_Pct_Norm != "NA") %>%
  filter(Human_Pct_Norm >= 1) %>%
  mutate(buckets = bucket(Human_Pct_Norm, 25)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')


colourCount = length(unique(bucket(sts_Df$HU_PCT, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

p1.1 <- sts_Df %>%
  filter(HU_PCT != "NA") %>%
  filter(HU_PCT >= 1) %>%
  mutate(buckets = bucket(HU_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p1.2 <- sts_Df %>%
  filter(LG_PCT != "NA") %>%
  filter(LG_PCT >= 1) %>%
  mutate(buckets = bucket(LG_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')


p2 <- stsSzCl_Df %>%
  filter(HU_PCT != "NA" & HU_PCT >= 1 & size_class != "Medium") %>%
  mutate(buckets = bucket(HU_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(size_class = factor(size_class, levels=c("Small", "Large"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ size_class, switch = "y")

p2.1 <- stsSzCl_Df %>%
  filter(LG_PCT != "NA" & LG_PCT >= 1 & size_class != "Medium") %>%
  mutate(buckets = bucket(LG_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(size_class = factor(size_class, levels=c("Small", "Large"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ size_class, switch = "y")


p3 <- stsSeason_Df %>%
  filter(HU_PCT != "NA" & HU_PCT >= 1) %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  mutate(buckets = bucket(HU_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Spring","Summer"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .01) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ seasons, switch = "y")

colourCount = length(unique(bucket(EcoSeason_Df$LG_PCT, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))

p3.1 <- stsSeason_Df %>%
  filter(LG_PCT != "NA" & LG_PCT >= 1) %>%
  filter(seasons != "Winter" & seasons != "Fall") %>%
  mutate(buckets = bucket(LG_PCT, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(seasons = factor(seasons, levels=c("Winter","Spring","Summer", "Fall"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .01) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ seasons, switch = "y")

ggsave(file = "Sts_FireCount_Hum.png", p1.1, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Sts_FireCount_Lig.png", p1.2, width = 5, height = 7, dpi=1000) #saves g

ggsave(file = "Sts_PctHum_SizeCl_Class.png", p2,  width = 8, height = 5, dpi=1000) #saves g
ggsave(file = "Sts_PctLght_SizeCl_Class.png", p2.1,  width = 8, height = 5, dpi=1000) #saves g

ggsave(file = "Sts_PctHum_Season_Class.png", p3, width = 8, height = 5, dpi=1000) #saves g
ggsave(file = "Sts_PctLig_Season_Class.png", p3.1, width = 8, height = 5, dpi=1000) #saves g



# ICS 209 and ecoregion assessment ----------------------------------------

ICS_red <- ICS[!duplicated(ICS$IncidentNu),]

wuw_ics209_cln <- ICS_red %>%
  mutate(costs = as.numeric(SuppressCo),
         Class = classify_new_categories(WUICLASS10)) %>%
  ungroup()

ICS_short <- wuw_short %>%
  select(ICS_209_INCIDENT_NUMBER, IGNITION, STUSPS) %>%
  right_join(., wuw_ics209_cln, by = c("ICS_209_INCIDENT_NUMBER" = "IncidentNu"))

ICS_short <- ICS_short[!is.na(ICS_short$IGNITION),]

St_Cost_209 <- ICS_short %>%
  group_by(STUSPS) %>%
  summarise(tot_cost = (sum(SuppressCo))) %>%
  ungroup()

St_l1 <- ICS_short %>%
  group_by(STUSPS, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(SuppressCo))) %>%
  left_join(., St_Cost_209, by = c("STUSPS")) %>%
  mutate(ignclass_tots = ifelse(is.na(ignclass_tots), 0, ignclass_tots),
         tot_cost = ifelse(is.na(tot_cost), 0, tot_cost),
         human_pct_cost = ifelse(is.na((ignclass_tots/(tot_cost)*100)), 0,
                                 round((ignclass_tots/(tot_cost)*100), digits =1)),
         stusps = STUSPS,
         ICS_Report = "Total CoSt")%>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

St_l2 <- ICS_short%>%
  group_by(STUSPS, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(SuppressCo))) %>%
  left_join(., St_Cost_209, by = c("STUSPS")) %>%
  spread(IGNITION, ignclass_tots) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human),
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Pct_Combine = (Human/(Human+Lightning))*100,
         tot_cost = ifelse(is.na(tot_cost), 0, tot_cost),
         stusps = STUSPS,
         ICS_Report = "Total CoSt") %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

St_l3 <- ICS_short%>%
  group_by(STUSPS, IGNITION) %>%
  summarise(ignclass_tots = (sum(SuppressCo))) %>%
  left_join(., St_Cost_209, by = c("STUSPS")) %>%
  mutate(ignclass_tots = ifelse(is.na(ignclass_tots), 0, ignclass_tots),
         tot_cost = ifelse(is.na(tot_cost), 0, tot_cost),
         human_pct_cost = ifelse(is.na((ignclass_tots/(tot_cost)*100)), 0,
                                 round((ignclass_tots/(tot_cost)*100), digits =1)),
         stusps = STUSPS,
         ICS_Report = "Total CoSt")

St_df_l1 <- left_join(st_df, St_l1, by = "stusps")
St_df_l1 <- na.omit(St_df_l1)

St_df_l2 <- left_join(st_df, St_l2, by = "stusps")
St_df_l2 <- na.omit(St_df_l2)

St_df_l3 <- left_join(st_df, St_l3, by = "stusps")

setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/States/ICS209")

colourCount8 = length(unique(bucket(St_df_l1$human_pct_cost, 10)))
getPalette8 = colorRampPalette(brewer.pal(9, "YlOrRd"))

p6 <- St_df_l1 %>%
  filter(human_pct_cost != "NA") %>%
  filter(human_pct_cost >= 1 & IGNITION != "Lightning") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette8(colourCount8), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class ~ IGNITION, switch="y")

colourCount2 = length(unique(bucket(St_df_l1$human_pct_cost, 10)))
getPalette2 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p65 <- St_df_l1 %>%
  filter(human_pct_cost >= 1 & IGNITION != "Human") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class ~ IGNITION, switch="y")

colourCount5 = length(unique(bucket(St_df_l3$human_pct_cost, 10)))
getPalette5 = colorRampPalette(brewer.pal(9, "YlOrRd"))

p64 <- St_df_l3 %>%
  filter(human_pct_cost != "NA" & IGNITION != "Lightning") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette5(colourCount5), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

colourCount4 = length(unique(bucket(St_df_l3$human_pct_cost, 10)))
getPalette4 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p63 <- St_df_l3 %>%
  filter(human_pct_cost != "NA"  & IGNITION != "Human") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette4(colourCount4), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

colourCount = length(unique(bucket(St_df_l2$Pct_Combine, 10)))
getPalette = colorRampPalette(brewer.pal(9, "RdYlBu"))

p62 <- St_df_l2 %>%
  filter(Pct_Combine != "NA") %>%
  filter(Pct_Combine >= 1) %>%
  mutate(buckets = bucket(Pct_Combine, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol = 1, strip.position = 'left')

grid.arrange(p6, p65, ncol = 2)
g <- arrangeGrob(p6, p65, ncol = 2) #generates g
ggsave(file = "PctCosts_StR_Class.png", g, width = 10, height = 6, dpi=1200) #saves g

grid.arrange(p64, p63, ncol = 1)
g <- arrangeGrob(p64, p63, ncol = 1) #generates g
ggsave(file = "PctCosts_StR_NoClass.png", g, width = 5, height = 7, dpi=1200) #saves g

g <- arrangeGrob(p62, ncol = ) #generates g
ggsave(file = "PctCostsCbm_StR_pClass.png", g, width = 5, height = 7, dpi=1200) #saves g

# Structures --------------------------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/States/ICS209")

ICS_red <- ICS[!duplicated(ICS$IncidentNu),]

wuw_ics209_cln <- ICS_red %>%
  mutate(resd = as.numeric(ResDestroy),
         Class = classify_new_categories(WUICLASS10)) %>%
  ungroup()

ICS_short <- wuw_short %>%
  select(ICS_209_INCIDENT_NUMBER, IGNITION, STUSPS) %>%
  right_join(., wuw_ics209_cln, by = c("ICS_209_INCIDENT_NUMBER" = "IncidentNu"))

ICS_short <- ICS_short[!is.na(ICS_short$IGNITION),]

St_stru_209 <- ICS_short %>%
  group_by(STUSPS) %>%
  summarise(tot_struc = (sum(ResDestroy))) %>%
  ungroup()

St_str_l1 <- ICS_short %>%
  group_by(STUSPS, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(ResDestroy))) %>%
  left_join(., St_stru_209, by = c("STUSPS")) %>%
  mutate(ignclass_tots = ifelse(is.na(ignclass_tots), 0, ignclass_tots),
         tot_struc = ifelse(is.na(tot_struc), 0, tot_struc),
         human_pct_cost = ifelse(is.na((ignclass_tots/(tot_struc)*100)), 0,
                                 round((ignclass_tots/(tot_struc)*100), digits =1)),
         stusps = STUSPS,
         ICS_Report = "Total CoSt")%>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

St_str_l2 <- ICS_short%>%
  group_by(STUSPS, IGNITION, Class) %>%
  summarise(ignclass_tots = (sum(ResDestroy))) %>%
  left_join(., St_stru_209, by = c("STUSPS")) %>%
  spread(IGNITION, ignclass_tots) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human),
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         Pct_Combine = ifelse(is.na((Human/(Human+Lightning))*100), 0, (Human/(Human+Lightning))*100),
         tot_struc = ifelse(is.na(tot_struc), 0, tot_struc),
         stusps = STUSPS,
         ICS_Report = "Total CoSt")

St_str_df_l1 <- left_join(st_df, St_str_l1, by = "stusps")
St_str_df_l2 <- left_join(st_df, St_str_l2, by = "stusps")


colourCount = length(unique(bucket(St_str_df_l1$human_pct_cost, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))

p621 <- St_str_df_l1 %>%
  filter(human_pct_cost >= 1 & IGNITION != "Lightning") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol = 1, strip.position = 'left')

colourCount2 = length(unique(bucket(St_str_df_l1$human_pct_cost, 10)))
getPalette2 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p622 <- St_str_df_l1 %>%
  filter(human_pct_cost >= 1 & IGNITION != "Human") %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol = 1, strip.position = 'left')

colourCount5 = length(unique(bucket(St_str_df_l1$human_pct_cost, 10)))
getPalette5 = colorRampPalette(brewer.pal(9, "YlOrRd"))

p64 <- St_str_df_l1 %>%
  filter(human_pct_cost != "NA" & IGNITION != "Lightning") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette5(colourCount5), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

colourCount4 = length(unique(bucket(St_str_df_l1$human_pct_cost, 10)))
getPalette4 = colorRampPalette(brewer.pal(9, "YlGnBu"))

p63 <- St_str_df_l1 %>%
  filter(human_pct_cost != "NA"  & IGNITION != "Human") %>%
  filter(human_pct_cost >= 1) %>%
  mutate(buckets = bucket(human_pct_cost, 10)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .1, size = .1)+
  coord_equal() +
  scale_fill_manual(values = getPalette4(colourCount4), name="Percent") + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ IGNITION, ncol = 1, strip.position = 'left')

grid.arrange(p621, p622, ncol = 2)
g <- arrangeGrob(p621, p622, ncol = 2) #generates g
ggsave(file = "PctStruct_StR_Class.png", g, width = 10, height = 6, dpi=1200) #saves g

grid.arrange(p64, p63, ncol = 1)
g <- arrangeGrob(p64, p63, ncol = 1) #generates g
ggsave(file = "PctStruct_StR_NoClass.png", g, width = 5, height = 7, dpi=1200) #saves g


# Area observed igntions proportional to the total class area using ****MTBS -  Human and lightning**** -------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/States")

wuw_st_polyhum <- wuw_mtbs_short %>%
  filter(IGNITION == "Human") %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  distinct(., BLK10, .keep_all = TRUE) %>%
  left_join(., area_grp_st, by = "STUSPS") %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")

st_ign_freq <- wuw_st_polyhum %>%
  group_by(STUSPS) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

st_poly_hum <- wuw_st_polyhum %>%
  group_by(STUSPS, Class, State_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(AREA_km2)) %>%
  ungroup() %>%
  left_join(., st_ign_freq, by = "STUSPS") %>%
  mutate(Human = ifelse(is.na(sum_cb), 0 ,sum_cb),
         human_pct = ifelse(Class == "VLD", (Human/(VLD))*100, 
                            ifelse(Class == "WUI", (Human/(WUI))*100,
                                   (Human/(Wildlands))*100)),
         PctSt_Hum = Human/State_Area_km2.y *100,
         PctAreaHum = ifelse(is.na(human_pct), 0, human_pct),
         stusps = STUSPS) %>%
  filter(VLD != "NA") 

wuw_st_polylig <- wuw_mtbs_short %>%
  filter(IGNITION == "Lightning") %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  distinct(., BLK10, .keep_all = TRUE) %>%
  left_join(., area_grp_st, by = "STUSPS") %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands")  

st_ign_freq <- wuw_st_polylig %>%
  group_by(STUSPS) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

st_poly_lig <- wuw_st_polylig %>%
  group_by(STUSPS, Class, State_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(AREA_km2)) %>%
  ungroup() %>%
  left_join(., st_ign_freq, by = "STUSPS") %>%
  mutate(Lightning = ifelse(is.na(sum_cb), 0 ,sum_cb),
         light_pct = ifelse(Class == "VLD", (Lightning/(VLD)*100), 
                            ifelse(Class == "WUI", (Lightning/(WUI)*100),
                                   (Lightning/(Wildlands)*100))),
         PctSt_Lig = Lightning/State_Area_km2.y *100,
         PctAreaLig = ifelse(is.na(light_pct), 0, light_pct),
         stusps = STUSPS) %>%
  filter(VLD != "NA")

st_poly_shp_hum <- left_join(st_df, st_poly_hum, by = "stusps")
st_poly_shp_lig <- left_join(st_df, st_poly_lig, by = "stusps")


p4.9 <- st_poly_shp_hum %>%
  filter(PctSt_Hum != "NA") %>%
  filter(PctSt_Hum >= 1) %>%
  mutate(buckets = bucket(PctSt_Hum, 1)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.0 <- st_poly_shp_lig %>%
  filter(PctSt_Lig != "NA") %>%
  filter(PctSt_Lig >= 1) %>%
  mutate(buckets = bucket(PctSt_Lig, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.1 <- st_poly_shp_hum %>%
  filter(PctAreaHum != "NA") %>%
  filter(PctAreaHum >= 1) %>%
  mutate(buckets = bucket(PctAreaHum, 2)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.2 <- st_poly_shp_lig %>%
  filter(PctAreaLig != "NA") %>%
  filter(PctAreaLig >= 1) %>%
  mutate(buckets = bucket(PctAreaLig, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

colourCount2 = length(unique(bucket(eco_poly_shp_lig$Lightning, 2000)))
getPalette2 = colorRampPalette(brewer.pal(9, "YlOrRd"))

p5.3 <- st_poly_shp_hum %>%
  filter(Human != "NA") %>%
  filter(Human >= 1) %>%
  mutate(buckets = bucket(Human, 2000)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") +
  #scale_fill_brewer(name="", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

colourCount = length(unique(bucket(eco_poly_shp_lig$Lightning, 5000)))
getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))

p5.4 <- st_poly_shp_lig %>%
  filter(Lightning != "NA") %>%
  filter(Lightning >= 1) %>%
  mutate(buckets = bucket(Lightning, 5000)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  scale_fill_manual(values = getPalette(colourCount), name="Percent") +
  #scale_fill_brewer(name="", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

ggsave(file = "streg_PctstBurn_Human_MTBS.png", p4.9, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "streg_PctstBurn_Lightning_MTBS.png", p5.0, width = 5, height = 7, dpi=1000) #saves g

ggsave(file = "streg_PctClassBurn_Human_MTBS.png", p5.1, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "streg_PctClassBurn_Lightning_MTBS.png", p5.2, width = 5, height = 7, dpi=1000) #saves g

ggsave(file = "streg_TotBurnArea_Human_MTBS.png", p5.3, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "streg_TotBurnArea_Lig_MTBS.png", p5.4, width = 5, height = 7, dpi=1000) #saves g

# Area observed igntions proportional to the total class area ***Short**** -------------------------------------------

wuw_sts_poly_hum <- wuw_short %>%
  filter(IGNITION != "Lighting") %>%
  mutate(AREA = as.numeric(AREA),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_st, by = "STUSPS")  

wuw_sts_poly_lig <- wuw_short %>%
  filter(IGNITION != "Human") %>%
  mutate(AREA = as.numeric(AREA),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_st, by = "STUSPS")  


stsPolyPropIGNITION_PctHum <- wuw_sts_poly_hum %>%
  group_by(STUSPS, IGNITION, Class, State_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(fire_size_km2)) %>%
  ungroup() %>%
  spread(IGNITION, sum_cb) %>%
  mutate(human_pct = ifelse(Class == "VLD", (Human/(VLD))*100, 
                            ifelse(Class == "WUI", (Human/(WUI))*100,
                                   (Human/(Wildlands))*100)),
         IGNITION = "Human") %>%
  select(STUSPS, Class, State_Area_km2.y, Wildlands, WUI, VLD, Human, human_pct)

stsPolyPropIGNITION_PctLig <- wuw_sts_poly_lig %>%
  group_by(STUSPS, IGNITION, Class, State_Area_km2.y, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(fire_size_km2)) %>%
  ungroup() %>%
  spread(IGNITION, sum_cb) %>%
  mutate(light_pct = ifelse(Class == "VLD", (Lightning/(VLD))*100, 
                            ifelse(Class == "WUI", (Lightning/(WUI))*100,
                                   (Lightning/(Wildlands))*100)),
         IGNITION = "Lightning") %>%
  select(STUSPS, Class, Lightning, light_pct)

sts_poly <- full_join(stsPolyPropIGNITION_PctHum, stsPolyPropIGNITION_PctLig, by = c("STUSPS", "Class")) %>%
  select(STUSPS, Class, State_Area_km2.y, Human, Lightning, human_pct, light_pct) %>%
  ungroup() %>%
  mutate(Lightning = ifelse(is.na(Lightning), 0 ,Lightning),
         Human = ifelse(is.na(Human), 0 ,Human),
         Pctsts_Hum = Human/State_Area_km2.y  *100,
         Pctsts_Lig = Lightning/State_Area_km2.y  *100,
         PctAreaHum = ifelse(is.na(human_pct), 0, human_pct),
         PctAreaLig = ifelse(is.na(light_pct), 0, light_pct),
         PctArea = PctAreaHum + PctAreaLig,
         stusps = STUSPS)

sts_poly_shp <- left_join(st_df, sts_poly, by = "stusps")

p5.12 <- sts_poly_shp %>%
  filter(Pctsts_Hum != "NA") %>%
  #filter(PctAreaHum >= 1) %>%
  mutate(buckets = bucket(Pctsts_Hum, 1)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.13 <- sts_poly_shp %>%
  filter(PctAreaLig != "NA") %>%
  #filter(PctAreaLig >= 1) %>%
  mutate(buckets = bucket(PctAreaLig, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

grid.arrange(p5.12, p5.13, ncol= 2)


p5.1 <- sts_poly_shp %>%
  filter(Pctsts_Hum != "NA") %>%
  filter(Pctsts_Hum >= 1) %>%
  mutate(buckets = bucket(Pctsts_Hum, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.2 <- sts_poly_shp %>%
  filter(Pctsts_Lig != "NA") %>%
  filter(Pctsts_Lig >= 1) %>%
  mutate(buckets = bucket(Pctsts_Lig, 5)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.3 <- sts_poly_shp %>%
  filter(Human != "NA") %>%
  filter(Human >= 1) %>%
  mutate(buckets = bucket(Human, 20000)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.4 <- sts_poly_shp %>%
  filter(Lightning != "NA") %>%
  filter(Lightning >= 1) %>%
  mutate(buckets = bucket(Lightning, 20000)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="", direction =1 , palette = "YlGnBu", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')


ggsave(file = "stsreg_PctClassBBurn_Human_Short.png", p5.1, width = 5, height = 7, dpi=1200) #saves g
ggsave(file = "stsreg_PctClassBurn_Lightning_Short.png", p5.2, width = 5, height = 7, dpi=1200) #saves g

ggsave(file = "stsreg_TotBurnArea_Human_Short.png", p5.3, width = 5, height = 6, dpi=1200) #saves g
ggsave(file = "stsreg_TotBurnArea_Lig_Short.png", p5.4, width = 5, height = 6, dpi=1200) #saves g

#Seasonal 
wuw_st_poly <- wuw_short %>%
  mutate(AREA = as.numeric(AREA_km2),
         fire_size_km2 = as.numeric(FIRE_SIZE * 0.00404686),
         Class = classify_new_categories(WUICLASS10),
         Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  left_join(., area_grp_st, by = "STUSPS")  

st_sum <- wuw_st_poly %>%
  group_by(STUSPS, Seasons) %>%
  summarize(sum_totfire = sum(fire_size_km2),
            tot_fire = n()) %>%
  ungroup() %>%  filter(STUSPS != "")

st_sum2 <- wuw_st_poly %>%
  group_by(STUSPS, Class, Seasons) %>%
  summarize(sum_totfire = sum(fire_size_km2),
            tot_fire = n()) %>%
  ungroup() %>% 
  filter(STUSPS != "")

St_Fden <- wuw_st_poly %>%
  group_by(STUSPS, VLD, Wildlands, WUI, IGNITION, Class, Seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  filter(VLD != "NA") %>%
  spread(IGNITION, n_fire) %>%
  left_join(., st_sum2, by = c("Class", "STUSPS", "Seasons")) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human), 
         Lightning = ifelse(is.na(Lightning), 0, Lightning),
         human_den = ifelse(Class == "VLD", ((tot_fire-Human)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Human)/(WUI)),
                                   ((tot_fire-Human)/(Wildlands)))),
         light_den = ifelse(Class == "VLD", ((tot_fire-Lightning)/(VLD)), 
                            ifelse(Class == "WUI", ((tot_fire-Lightning)/(WUI)),
                                   ((tot_fire-Lightning)/(Wildlands)))),
         f_den = (1-(human_den/(human_den+light_den)))*100,
         stusps = STUSPS)

st_den_shp <- left_join(st_df, St_Fden, by = "stusps")

St_Pct <- wuw_st_poly %>%
  group_by(STUSPS, State_Area_km2.x, IGNITION, Class, Seasons) %>%
  summarize(sum_fire = sum(fire_size_km2)) %>%
  ungroup() %>%
  filter(State_Area_km2.x != "NA") %>%
  left_join(., st_sum, by = c("STUSPS", "Seasons")) %>%
  spread(IGNITION, sum_fire) %>%
  mutate(Human = ifelse(is.na(Human), 0, Human), 
         Lightning = ifelse(is.na(Lightning), 0, Lightning), 
         Human_Norm_EcoR = (Human/sum_totfire),
         Lightning_Norm_EcoR = (Lightning/sum_totfire),
         Human_Pct_Norm = Human_Norm_EcoR/(Human_Norm_EcoR+Lightning_Norm_EcoR)*100,
         Lightning_Pct_Norm = Human_Pct_Norm - 100,
         HU_PCT = ((Human/sum_totfire)*100),
         LG_PCT = ((Lightning/sum_totfire)*100),
         stusps = STUSPS)

St_Df2 <- left_join(st_df, St_Pct, by = "stusps")

colourCount2 = length(unique(bucket(st_den_shp$f_den, 10)))
getPalette2 = colorRampPalette(c("blue", "white","red"),
                               space = "Lab")

p100 <- st_den_shp %>%
  filter(f_den != "NA") %>%
  filter(f_den >= 1) %>%
  filter(Seasons == "Spring" | Seasons == "Summer") %>%
  mutate(buckets = bucket(f_den, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Percent fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16),
        legend.position="none") +
  facet_grid(Class~ Seasons, switch = "y")

colourCount2 = length(unique(bucket(St_Df2$Human_Pct_Norm, 10)))
getPalette2 = colorRampPalette(c("blue", "white","red"),
                               space = "Lab")

p1 <- Eco_Df2 %>%
  filter(Human_Pct_Norm != "NA") %>%
  filter(Human_Pct_Norm >= 1) %>%
  filter(Seasons == "Spring" | Seasons == "Summer") %>%
  mutate(buckets = bucket(Human_Pct_Norm, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=ecoregions, aes(x=long,y=lat,group=group), color='gray25', fill = NA, size = .1)+
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette2(colourCount2), name="Percent") + 
  ggtitle('(B) Percent burn area') +
  theme(plot.title = element_text(hjust = 0, size = 16, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_grid(Class~ Seasons, switch = "y")
grid.arrange(p100, p1, ncol= 2)

# MTBS  ----------------------------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/States")

wuw_mtbs_slim <- wuw_mtbs %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(State_Area_km2 != "") %>%
  left_join(., area_grp_st, by = "STUSPS")
#levels(wuw_mtbs_slim$Class)[levels(wuw_mtbs_slim$Class)=="VLD"] <- "WUI"

StPolyM <- wuw_mtbs_slim %>%
  group_by(STUSPS, Class, State_Area_km2.x, Wildlands, WUI, VLD) %>%
  summarize(sum_cb = sum(AREA_km2)) %>%
  ungroup() %>%
  mutate(burn_pct_class = ifelse(Class == "VLD", (sum_cb/(VLD))*100, 
                                 ifelse(Class == "WUI", (sum_cb/(WUI))*100,
                                        (sum_cb/(Wildlands))*100)),
         burn_pct_st = (sum_cb/(State_Area_km2.x))*100,
         stusps = STUSPS)

st_poly_m <- left_join(st_df, StPolyM, by = "stusps")

colourCount = length(unique(bucket(st_poly_m$burn_pct_class, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
p4.91 <- st_poly_m %>%
  filter(burn_pct_class != "NA") %>%
  #filter(burn_pct_class >= 1) %>%
  mutate(buckets = bucket(burn_pct_class, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

#Only BAECV burned area 
colourCount = length(unique(bucket(st_poly_b$burn_pct_class, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
p4.91 <- st_poly_b %>%
  filter(burn_pct_class != "NA") %>%
  filter(burn_pct_class >= 1) %>%
  mutate(buckets = bucket(burn_pct_class, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')


#Both BAECV and MTBS burned area 
colourCount = length(unique(bucket(st_poly_bm$burn_pct_class, 10)))
getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
p4.9 <- st_poly_bm %>%
  filter(burn_pct_class != "NA") %>%
  filter(burn_pct_class >= 1) %>%
  mutate(buckets = bucket(burn_pct_class, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  scale_fill_manual(values = getPalette(colourCount), name="Percent") + 
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p5.0 <- st_poly_bm %>%
  filter(burn_pct_st != "NA") %>%
  filter(burn_pct_st >= 1) %>%
  mutate(buckets = bucket(burn_pct_st, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = factor(buckets))) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=16)) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

ggsave(file = "Streg_PctClassBurn_BAECV.png", p4.91, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Streg_PctClassBurn_BAECV_MTBS.png", p4.9, width = 5, height = 7, dpi=1000) #saves g
ggsave(file = "Streg_PctStBurn_BAECV.png", p5.0, width = 5, height = 7, dpi=1000) #saves g

# wuw_st_poly <- wuw_mtbs %>%
#   mutate(AREA = as.numeric(AREA),
#          Class = classify_new_categories(WUICLASS10)) %>%
#   left_join(., stnorm, by = "STUSPS")  
# 
# stPolyPropIGNITION <- wuw_st_poly %>%
#   group_by(STUSPS, Class, State_Km2, Wild_km2, WUI_km2, VLD_km2) %>%
#   summarize(sum_cb = sum(AREA)) %>%
#   ungroup() %>%
#   mutate(burn_pct_class = ifelse(Class == "VLD", (sum_cb/(VLD_km2))*100, 
#                                  ifelse(Class == "WUI", (sum_cb/(WUI_km2))*100,
#                                         (sum_cb/(Wild_km2))*100)),
#          burn_pct_st = (sum_cb/(State_Km2))*100,
#          stusps = STUSPS)
# 
# 
# st_poly_shp <- left_join(st_df, stPolyPropIGNITION, by = "stusps") 
# 
# p4.9 <- st_poly_shp %>%
#   filter(burn_pct_class != "NA") %>%
#   filter(burn_pct_class >= 1) %>%
#   mutate(buckets = bucket(burn_pct_class, 10)) %>%
#   transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
#   ggplot(aes(long, lat, group = group)) +
#   geom_polygon(aes(fill = factor(buckets))) +
#   geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
#   geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
#   coord_equal() + 
#   #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
#   scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
#   theme_nothing(legend = TRUE) +
#   #labs(title = "%Area that observed\n Lightning IGNITIONs") +
#   theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
#         strip.background=element_blank(),
#         strip.text = element_text(face="bold", size=16)) +
#   facet_wrap( ~ Class, ncol=1, strip.position = 'left')
# 
# p5.0 <- st_poly_shp %>%
#   filter(burn_pct_st != "NA") %>%
#   filter(burn_pct_st >= 1) %>%
#   mutate(buckets = bucket(burn_pct_st, 5)) %>%
#   transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
#   ggplot(aes(long, lat, group = group)) +
#   geom_polygon(aes(fill = factor(buckets))) +
#   geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
#   geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
#   coord_equal() + 
#   #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
#   scale_fill_brewer(name="Percent", direction =1 , palette = "YlOrRd", na.value= "mintcream") +
#   theme_nothing(legend = TRUE) +
#   #labs(title = "%Area that observed\n Lightning IGNITIONs") +
#   theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
#         strip.background=element_blank(),
#         strip.text = element_text(face="bold", size=16)) +
#   facet_wrap( ~ Class, ncol=1, strip.position = 'left')
# 
# ggsave(file = "streg_PctClassBurn_MTBS.png", p4.9, width = 5, height = 7, dpi=1000) #saves g
# ggsave(file = "streg_PctstBurn_MTBS.png", p5.0, width = 5, height = 7, dpi=1000) #saves g


# States - the max seasonality of fire occurence  ----------------------

stswuw_seasonality_prep <- wuw_short %>%
  filter(Class != "Urban")%>%
  mutate(fire_size_km2 = (FIRE_SIZE * 0.00404686),
         size_class = classify_fire_size(fire_size_km2),
         seasons = classify_seasons(DISCOVERY_DOY),
         Class = classify_new_categories(WUICLASS10)) %>%
  left_join(., stnorm, by = "STUSPS")  

stswuw_short_seasonality <- stswuw_seasonality_prep %>%
  group_by(STUSPS, Class, IGNITION, seasons) %>%
  summarize(n_count = n()) %>%
  ungroup %>%
  spread(seasons, n_count) %>%
  group_by(STUSPS, Class, IGNITION) %>%
  do(get_month_max(.)) %>%
  mutate(stusps = STUSPS)

sts_seasonality_shp <- left_join(st_df, stswuw_short_seasonality, by = "stusps")


p25 <- sts_seasonality_shp %>%
  filter(Class != "NA")%>%
  #mutate(buckets = bucket(PctBurn, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI","VLD","Wildlands"))) %>%
  transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = max_season)) +
  geom_polygon(aes(x=long,y=lat,group=group), color = 'black', alpha = .1, size = .1) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray25', fill = NA, alpha = .5, size = .1)+
  coord_equal() + 
  #scale_fill_distiller("bottomright", name="Percent", limits=c(0, 100), direction =1 , palette = "YlOrRd", breaks = pretty_breaks(n=5), na.value= "mintcream") +
  scale_fill_brewer(position ="left", name="Seasons", direction =-1 , palette = "BrBG", na.value= "mintcream") +
  theme_nothing(legend = TRUE) +
  #labs(title = "%Area that observed\n Lightning IGNITIONs") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face="bold"),
        strip.background=element_blank(),
        strip.text = element_text(face="bold", size=14)) +
  facet_grid(Class ~ IGNITION, switch="y")
ggsave(file = "streg_MaxSeasonality_Class.png", p25, width = 8, height = 5, dpi=1200) #saves g



#######################################################


# What is the total human ignitions relative to the total ignitions in Short --------
wuw_short_cln <- wuw_short %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  distinct(., FPA_ID, .keep_all = TRUE)

wuw_short_ctots <- wuw_short_cln %>%
  group_by(Class) %>%
  summarize(tot_fire = n()) %>%
  ungroup() 

short_sums <- wuw_short_cln %>%
  group_by(IGNITION, Class) %>%
  summarize(tot_ign = n()) %>%
  left_join(., wuw_short_ctots, by = "Class") %>%
  mutate(tot_pct = tot_ign/tot_fire*100)


p3 <- short_sums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = tot_pct, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  theme(legend.position="top") +
  xlab("") + 
  ylab("Fire frequency (%)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

p3.5 <- short_sums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = tot_ign, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Total fire frequecy") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")


# Stats for the paper -----------------------------------------------------
#What is the percent of fires started by humans in the WUI relative to all of the ignitions in the WUI, VLDH, and wildlands?
Short_stats <- wuw_short_cln %>%
  group_by(IGNITION, Class) %>%
  summarize(tot_fire = n(),
            sum_fire = sum(fire_size_km2)) %>%
  ungroup() 


#What is the proportion of burned area in the Short data relative to the MTBS, regardless of ignition?
wuw_mtbs_cln <- wuw_mtbs_short %>%
  mutate(AREA = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Class) %>%
  summarize(MTBS_FireFreq = n(),
            MTBS_FireArea = sum(AREA)) %>%
  ungroup()

short_sums_stats <- wuw_short_cln %>%
  group_by(Class) %>%
  summarize(Short_FireFreq = n(),
            Short_FireAra = sum(fire_size_km2)) %>%
  left_join(., wuw_mtbs_cln, by = "Class") %>%
  mutate(Percent_Class = MTBS_FireArea/Short_FireAra*100)



# What is the total area burned relative to the proportion burned by MTBS --------

wuw_bae_cln <- bae %>%
  mutate(AREA_km2 = as.numeric(AREA_km2),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%

wuw_bae_ctots <- wuw_bae_cln %>%
  group_by(Class) %>%
  summarize(sum_fire = sum(AREA_km2)) %>%
  ungroup() 

bae_sums <- wuw_bae_cln %>%
  group_by(IGNITION, Class) %>%
  summarize(sum_ign = sum(AREA_km2)) %>%
  left_join(., wuw_bae_ctots, by = "Class") %>%
  mutate(sum_pct = (sum_ign/sum_fire)*100)

p4 <- bae_sums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = sum_pct, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Burned landcover class (%)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

p4.5 <- bae_sums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(IGNITION = factor(IGNITION, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = sum_ign, fill = IGNITION),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Total burn area (km2)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

# Costs -------------------------------------------------------------------
wuw_ics209_cln <- ICS %>%
  distinct(., IncidentNu, Year, .keep_all = TRUE) %>%
  mutate(costs = as.numeric(SuppressCo),
         destroy = as.numeric(ResDestroy),
         threat = as.numeric(ResThreat),
         deaths = as.numeric(Fatalities),
         Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 
  
wuw_ics209_ctots <- wuw_ics209_cln %>%
  group_by(Class) %>%
  summarize(TotalCosts = sum(costs)) %>%
  ungroup() 

ics209_costs <- wuw_ics209_cln %>%
  group_by(Ignition, Class) %>%
  summarize(Class_Costs = sum(costs)) %>%
  left_join(., wuw_ics209_ctots, by = "Class") %>%
  mutate(sum_pct = Class_Costs/TotalCosts*100) 

p5 <- ics209_costs %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = sum_pct, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Fire suppression costs (%)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none") 

p5.5 <- ics209_costs %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_Costs/1000000000, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Total accrued costs (in billions of dollars)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")


# Structures --------------------------------------------------------------

wuw_ics209_sctots <- wuw_ics209_cln %>%
  group_by(Class) %>%
  summarize(struct_dest = sum(destroy),
            struct_threat = sum(threat)) %>%
  ungroup() 

ics209_strsums <- wuw_ics209_cln %>%
  group_by(Ignition, Class) %>%
  summarize(res_destroy_per = sum(destroy),
            res_threat_per = sum(threat)) %>%
  left_join(., wuw_ics209_sctots, by = "Class") %>%
  ungroup() %>%
  mutate(destroy_pct = (res_destroy_per/struct_dest)*100,
         threat_pct = (res_threat_per/struct_threat)*100) 

p6 <- ics209_strsums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = destroy_pct, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Structures lost (%)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

p6.5 <- ics209_strsums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = res_destroy_per, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Total structures lost") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

p7 <- ics209_strsums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = threat_pct, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Structures threatened (%)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

p7.5 <- ics209_strsums %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = res_threat_per, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Total structures threatened") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

# Fatalities --------------------------------------------------------------
wuw_ics209_ctots <- wuw_ics209_cln %>%
  group_by(Class) %>%
  summarize(death_sum = sum(Fatalities)) %>%
  ungroup() 

ics209_fat <- wuw_ics209_cln %>%
  group_by(Ignition, Class) %>%
  summarize(Class_Fat = sum(Fatalities)) %>%
  left_join(., wuw_ics209_ctots, by = "Class") %>%
  mutate(death_pct = (Class_Fat/death_sum)*100)


p8 <- ics209_fat %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = death_pct, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Lives lost (%)") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")

p8.5 <- ics209_fat %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(Ignition = factor(Ignition, levels=c("Lightning", "Human"))) %>%
  ggplot() +
  geom_bar(aes(x = Class, y = Class_Fat, fill = Ignition),
           stat="identity", color = "black") + 
  scale_fill_manual(values=c("white", "dark gray")) +
  scale_y_continuous(labels = comma) +
  xlab("") + 
  ylab("Total lives lost") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position="none")


# Save figures to png -----------------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/WUIFire/WorkOut/Percentages/Lise_P_FAMWeb")

grid.arrange(p3.5, p4.5, p5.5, p6.5, p7.5, p8.5, ncol=6)
g <- arrangeGrob(p3.5, p4.5, p5.5, p6.5, p7.5, p8.5, ncol=6) #generates g
ggsave(file = "PercentIgnition_againstTotals.png", g, width =15, height = 5, dpi=1200) #saves g


grid.arrange(p3, p4, p5, p6,p7,p8, ncol=6)
g <- arrangeGrob(p3, p4, p5, p6,p7,p8, ncol=6) #generates g
ggsave(file = "PercentIgnition.png", g, width =13, height = 5, dpi=1200) #saves g

# BAE Timeseries ---------------------------------------------------------
mtbs_p <- bae %>%
  mutate(Class = classify_new_categories(WUICLASS10)) %>%
  filter(Class == "WUI" | Class == "Wildlands") %>%
  group_by(NA_L1NAME, Class, FIRE_YEAR, IGNITION) %>%
  summarise(f_cnt = n(),
            b_area = sum(AREA_km2)) %>%
  ungroup()

mtbs_p%>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = FIRE_YEAR, y = b_area, color = IGNITION)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", se=FALSE) +
  scale_y_log10(labels = comma) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Fire Year") + ylab("Burned area") +
  theme_pub()  +
  theme(legend.position="none") + 
  facet_grid(Class ~ NA_L1NAME)

ics_pd <- wuw_ics209_cln %>%
  filter(Class == "WUI") %>%
  group_by(Class, Ignition, Year) %>%
  summarise(costs = sum(costs),
            destroy = sum(destroy),
            threat = sum(threat)) %>%
  ungroup()

p_c <- ics_pd%>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = Year, y = costs/100000000, color = Ignition)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", se=FALSE) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Fire Year") + ylab("Fire supression costs (in billions of dollars)") +
  theme_pub()  +
  theme(legend.position="none")

p_d <- ics_pd%>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = Year, y = destroy, color = Ignition)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", se=FALSE) +
  scale_y_log10(labels = comma) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Fire Year") + ylab("Homes destroyed") +
  theme_pub()  +
  theme(legend.position="none") 

p_t <- ics_pd%>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = Year, y = threat, color = Ignition)) +
  geom_point( size = 1, alpha = 0.5) + 
  geom_smooth(method = "glm", se=FALSE) +
  scale_y_log10(labels = comma) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Fire Year") + ylab("Homes threatened") +
  theme_pub()  +
  theme(legend.position="none") 
grid.arrange(p_c, p_d, p_t, ncol = 3)


