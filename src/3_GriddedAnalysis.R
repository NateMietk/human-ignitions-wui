# Fire return interval -----------****Fishnet   ----------------------------------------------------------------------------
results_gridded = file.path("results", "Gridded")

FireMetrics <- wuw_eco_poly %>%
  group_by(FishID_25k, Class, IGNITION, FIRE_YEAR) %>%
  summarize(tot_fire = n()) %>%
  mutate(FireInterval = c(NA, diff(FIRE_YEAR)),
         FireInterval = ifelse(is.na(FireInterval), 0, FireInterval)) %>%
  filter(FireInterval != 0) %>%
  group_by(FishID_25k, Class, IGNITION) %>%
  summarise(num_yr = n(),
            Sum_tot_fire = sum(tot_fire),
            MeanFireInterval = ifelse(is.na(mean(FireInterval)), 0 , mean(FireInterval)),
            MeanOccurPerYear = (Sum_tot_fire/MeanFireInterval),
            FirepYear = (Sum_tot_fire/num_yr),
            ptsz_n = classify_ptsize_breaks(Sum_tot_fire),
            FPY = classify_returninterval(FirepYear),
            MOPY = classify_occpyear(MeanOccurPerYear),
            FRI = classify_returninterval(MeanFireInterval)) %>%
  ungroup() %>%
  left_join(., wuw_eco_poly, by = "FishID_25k") %>%
  distinct(FishID_25k, Class.x, IGNITION.x, .keep_all = TRUE) %>%
  mutate(Class = Class.x,
         IGNITION = IGNITION.x)

tot_fpy <- FireMetrics %>%
  group_by(FishID_25k, Class) %>%
  summarise(tot_fpy = sum(FirepYear))

FPY_Den <- FireMetrics %>%
  select(FishID_25k, IGNITION, Class, FirepYear) %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  spread(IGNITION, FirepYear) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human), 
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         tot_fire = (n_Human + n_Lightning),
         n_den = (1-(n_Lightning/tot_fire))*100) %>%
  select(FishID_25k, Class, tot_fire, n_Human, n_Lightning, n_den) %>%
  left_join(., tot_fpy, by = c("FishID_25k", "Class")) %>%
  mutate(FPY = classify_returninterval(tot_fpy))

Den_FPY <- left_join(fs25_df, FPY_Den, by = "FishID_25k")
GA_FireMetrics <- left_join(fs25_df, FireMetrics, by = "FishID_25k")

colourCount_fpy = length(unique(bucket(Den_FPY$n_den, 10)))
colourCoun_fpy = length(unique(bucket(Den_FPY$s_den, 10)))
getPalette_fpy = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")

p100_FPY <- Den_FPY %>%
  filter(Class != "VLD") %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(FPY = factor(FPY, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = FPY), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_fpy(colourCount_fpy), name="Percent") + 
  scale_size_discrete(range = c(.3,.75), name="Fire per year") +
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p100a <- p100_FPY + theme(legend.position="none")
g <- arrangeGrob(p100a, ncol = 1) #generates g
ggsave(file = file.path(results_gridded, "Gridded_FF_FPY.png"), g, width = 4, height = 5, dpi=1200) #saves g
ggsave(file = file.path(results_gridded, "Gridded_FF_FPY.EPS"), g, width = 8.7, height = 10, dpi=600, scale = 1, units = "cm") #saves g

legend <- g_legend(p100_FPY) 
ggsave(file = file.path(results_gridded, "legend", "Gridded_FF_FPY_Legend.png"), legend, width = 2, height = 3.5, dpi=1200) #saves g

p100 <- GA_FireMetrics %>%
  filter(Class != "VLD") %>%
  filter(IGNITION == "Human") %>%
  transform(FRI = factor(FRI, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(FRI), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("0 - 2" = "#D62728", 
                                "2 - 5" = "#1F77B4", 
                                "5 - 10" =  "#FF7F0E", 
                                "> 10" = "#2CA02C"),                       
                     name="Fire return interval") +  
  scale_size_discrete(range = c(0.3,.9),
                      name = "Total # Fires") +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Mean fire return interval') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = 'y')

p101 <- GA_FireMetrics %>%
  filter(Class != "VLD") %>%
  filter(IGNITION =="Lightning") %>%
  transform(FRI = factor(FRI, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(FRI), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("0 - 2" = "#D62728", 
                                "2 - 5" = "#1F77B4", 
                                "5 - 10" =  "#FF7F0E", 
                                "> 10" = "#2CA02C"),                      
                     name="Fire return interval") +    
  scale_size_discrete(range = c(0.3,.9), 
                      name = "Total # Fires") +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = 'y')

p100a <- p100 + theme(legend.position="none")
p101a <- p101 + theme(legend.position="none")

grid.arrange(p100a, p101a, ncol =2)
g <- arrangeGrob(p100a, p101a, ncol = 2) #generates g
ggsave(file = "Gridded_FRI.png", g, width = 8, height = 5, dpi=1200) #saves g
legend <- g_legend(p101) 
ggsave(file = "./legend/Gridded_FRI_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g


p104 <- GA_FireMetrics %>%
  filter(Class != "VLD") %>%
  filter(IGNITION == "Human") %>%
  transform(FPY = factor(FPY, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(FPY), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("0 - 2" = "#1F77B4", 
                                "2 - 5" = "#2CA02C", 
                                "5 - 10" =  "#FF7F0E", 
                                "> 10" = "#D62728"),                       
                     name="Mean # fires\n per fire year") +  
  scale_size_discrete(range = c(0.3,.9),
                      name = "Total # Fires") +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Mean fire occurrence per fire year') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = 'y')

p105 <- GA_FireMetrics %>%
  filter(Class != "VLD") %>%
  filter(IGNITION =="Lightning") %>%
  transform(FPY = factor(FPY, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = factor(FPY), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("0 - 2" = "#1F77B4", 
                                "2 - 5" = "#2CA02C", 
                                "5 - 10" =  "#FF7F0E", 
                                "> 10" = "#D62728"),                      
                     name="Mean # fires\n per fire year") +    
  scale_size_discrete(range = c(0.3,.9), 
                      name = "Total # Fires") +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = 'y')

p104a <- p104 + theme(legend.position="none")
p105a <- p105 + theme(legend.position="none")

grid.arrange(p104a, p105a, ncol =2)
g <- arrangeGrob(p104a, p105a, ncol = 2) #generates g
ggsave(file = "Gridded_FPY.png", g, width = 8, height = 5, dpi=1200) #saves g
legend <- g_legend(p104) 
ggsave(file = "./legend/Gridded_FPY_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g

#

# Fire return interval timeseries ****Data Prep *Theil-Sen ---------------------------------------------------------
FireMetrics_f <- wuw_eco_poly %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(tot_fire = n(),
            Fseas = IQR(DISCOVERY_DOY))

FireMetrics_b <- wuw_eco_bae %>%
  group_by(IGNITION, Class, FIRE_YEAR) %>%
  summarize(bae = sum(AREA_km2)) %>%
  ungroup() %>%
  left_join(., FireMetrics_f, by = c("Class", "IGNITION", "FIRE_YEAR")) %>%
  mutate(IgCl = paste(IGNITION, Class, sep = "_"))

FireMetrics_py <- wuw_eco_poly %>%
  group_by(FishID_10k, Class, IGNITION, FIRE_YEAR) %>%
  summarize(tot_fire = n(),
            MeanFireSize = mean(fire_size_km2)) %>%
  mutate(FireInterval = c(NA, diff(FIRE_YEAR)),
         FireInterval = ifelse(is.na(FireInterval), 0, FireInterval)) %>%
  filter(FireInterval != 0) %>%
  group_by(Class, IGNITION, FIRE_YEAR) %>%
  summarise(tot_fire = sum(tot_fire),
            FPY = (tot_fire/12)) %>%
  ungroup() %>%
  mutate(IgCl = paste(IGNITION, Class, sep = "_"))

H_WUI_fpy <- FireMetrics_py %>%
  filter(IgCl == "Human_WUI") %>%
  do(fit_mblm_fpy(.))

H_VLD_fpy <- FireMetrics_py %>%
  filter(IgCl == "Human_VLD") %>%
  do(fit_mblm_fpy(.))

H_Wildlands_fpy <- FireMetrics_py %>%
  filter(IgCl == "Human_Wildlands") %>%
  do(fit_mblm_fpy(.))

L_WUI_fpy <- FireMetrics_py %>%
  filter(IgCl == "Lightning_WUI") %>%
  do(fit_mblm_fpy(.))

L_VLD_fpy <- FireMetrics_py %>%
  filter(IgCl == "Lightning_VLD") %>%
  do(fit_mblm_fpy(.))

L_Wildlands_fpy <- FireMetrics_py %>%
  filter(IgCl == "Lightning_Wildlands") %>%
  do(fit_mblm_fpy(.))

H_WUI <- FireMetrics_b %>%
  filter(IgCl == "Human_WUI") %>%
  do(fit_mblm(.))

H_VLD <- FireMetrics_b %>%
  filter(IgCl == "Human_VLD") %>%
  do(fit_mblm(.))

H_Wildlands <- FireMetrics_b %>%
  filter(IgCl == "Human_Wildlands") %>%
  do(fit_mblm(.))

L_WUI <- FireMetrics_b %>%
  filter(IgCl == "Lightning_WUI") %>%
  do(fit_mblm(.))

L_VLD <- FireMetrics_b %>%
  filter(IgCl == "Lightning_VLD") %>%
  do(fit_mblm(.))

L_Wildlands <- FireMetrics_b %>%
  filter(IgCl == "Lightning_Wildlands") %>%
  do(fit_mblm(.))

H_WUI_bae <- FireMetrics_b %>%
  filter(IgCl == "Human_WUI") %>%
  do(fit_mblm_bae(.))

H_VLD_bae <- FireMetrics_b %>%
  filter(IgCl == "Human_VLD") %>%
  do(fit_mblm_bae(.))

H_Wildlands_bae <- FireMetrics_b %>%
  filter(IgCl == "Human_Wildlands") %>%
  do(fit_mblm_bae(.))

L_WUI_bae <- FireMetrics_b %>%
  filter(IgCl == "Lightning_WUI") %>%
  do(fit_mblm_bae(.))

L_VLD_bae <- FireMetrics_b %>%
  filter(IgCl == "Lightning_VLD") %>%
  do(fit_mblm_bae(.))

L_Wildlands_bae <- FireMetrics_b %>%
  filter(IgCl == "Lightning_Wildlands") %>%
  do(fit_mblm_bae(.))

H_WUI_fsl <- FireMetrics_b %>%
  filter(IgCl == "Human_WUI") %>%
  do(fit_mblm_fsl(.))

H_VLD_fsl <- FireMetrics_b %>%
  filter(IgCl == "Human_VLD") %>%
  do(fit_mblm_fsl(.))

H_Wildlands_fsl <- FireMetrics_b %>%
  filter(IgCl == "Human_Wildlands") %>%
  do(fit_mblm_fsl(.))

L_WUI_fsl <- FireMetrics_b %>%
  filter(IgCl == "Lightning_WUI") %>%
  do(fit_mblm_fsl(.))

L_VLD_fsl <- FireMetrics_b %>%
  filter(IgCl == "Lightning_VLD") %>%
  do(fit_mblm_fsl(.))

L_Wildlands_fsl <- FireMetrics_b %>%
  filter(IgCl == "Lightning_Wildlands") %>%
  do(fit_mblm_fsl(.))

mblm_ign_long <- rbind(H_WUI, L_WUI, H_VLD, L_VLD, H_Wildlands, L_Wildlands) %>%
  mutate(IgCl = paste(IGNITION, Class, sep= "_")) %>%
  arrange(desc(pval))
mblm_bae_long <- rbind(H_WUI_bae, L_WUI_bae, H_VLD_bae, L_VLD_bae, H_Wildlands_bae, L_Wildlands_bae) %>%
  mutate(IgCl = paste(IGNITION, Class, sep= "_")) %>%
  arrange(desc(pval))
mblm_fsl_long <- rbind(H_WUI_fsl, L_WUI_fsl, H_VLD_fsl, L_VLD_fsl, H_Wildlands_fsl, L_Wildlands_fsl) %>%
  mutate(IgCl = paste(IGNITION, Class, sep= "_")) %>%
  arrange(desc(pval))
mblm_fpy_long <- rbind(H_WUI_fpy, L_WUI_fpy, H_VLD_fpy, L_VLD_fpy, H_Wildlands_fpy, L_Wildlands_fpy) %>%
  mutate(IgCl = paste(IGNITION, Class, sep= "_")) %>%
  arrange(desc(pval))

# Fire characteristics timeseries ****Data plot *Theil-Sen -------------------------------
FF_p <- FireMetrics_b %>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(FIRE_YEAR, tot_fire, colour = IGNITION)) +
  geom_point(data=subset(FireMetrics_b,
                         IgCl == "Human_VLD" | 
                           IgCl == "Lightning_VLD" | 
                           IgCl == "Human_Wildlands" |
                           IgCl == "Lightning_Wildlands"),
             na.rm=TRUE, size = 0.5, alpha = 0.5) + 
  geom_point(data=subset(FireMetrics_b, IgCl == "Human_WUI"),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="#FF7F0E") + 
  geom_point(data=subset(FireMetrics_b, IgCl == "Lightning_WUI"),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="gray75") +           
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Human_VLD" | 
                          IgCl == "Lightning_VLD" | 
                          IgCl == "Human_Wildlands" |
                          IgCl == "Lightning_Wildlands"), 
    method = "glm", method.args = list(family = "poisson"), size=0.75) + 
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Human_WUI" ),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("#FF7F0E", 0.5)) +
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Lightning_WUI"),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("gray75", 0.5)) +
  scale_size_identity() +
  scale_color_manual(values=c("red3", "black")) +
  xlab("Fire year") + ylab("Fire frequency") +
  theme_pub() +
  theme(plot.title = element_text(hjust = 0, size = 12),
            #axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            strip.background=element_blank(),
            strip.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 12),
            legend.key = element_rect(fill = "white")
            ,legend.position = "none") + 
  facet_grid( ~ Class)

FPY_p <- FireMetrics_py %>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(FIRE_YEAR, FPY, colour = IGNITION)) +
  geom_point(data=subset(FireMetrics_py,
                         IgCl == "Lightning_WUI" | 
                           IgCl == "Lightning_VLD"),
             na.rm=TRUE, size = 0.5, alpha = 0.5) + 
  geom_point(data=subset(FireMetrics_py, IgCl == "Human_WUI" |
                           IgCl == "Human_VLD" |
                           IgCl == "Human_Wildlands"),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="#FF7F0E") + 
  geom_point(data=subset(FireMetrics_py, IgCl == "Lightning_Wildlands"),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="gray75") +  
  stat_smooth(data=subset(FireMetrics_py,
                          IgCl == "Lightning_WUI" | 
                            IgCl == "Lightning_VLD"), 
              method = "glm", method.args = list(family = "poisson"), size=0.75) + 
  stat_smooth(data=subset(FireMetrics_py,
                          IgCl == "Human_WUI" |
                          IgCl == "Human_VLD" |
                          IgCl == "Human_Wildlands"),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("#FF7F0E", 0.5)) +
  stat_smooth(data=subset(FireMetrics_py,
                          IgCl == "Lightning_Wildlands"),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("gray75", 0.5)) +
  scale_size_identity() +
  scale_color_manual(values=c("black")) +
  xlab("Fire year") + ylab("Mean number of wildfires per year") +
  theme_pub() +
  theme(plot.title = element_text(hjust = 0, size = 12),
        #axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none"
  ) +
  facet_grid( ~ Class)

BAE_p <- FireMetrics_b %>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(FIRE_YEAR, bae, colour = IGNITION)) +
  geom_point(data=subset(FireMetrics_b,
                         IgCl == "Human_WUI" | 
                           IgCl == "Lightning_WUI" | 
                           IgCl == "Human_VLD" |
                           IgCl == "Lightning_VLD"),
             na.rm=TRUE, size = 0.5, alpha = 0.5) + 
  geom_point(data=subset(FireMetrics_b, IgCl == "Human_Wildlands" ),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="#FF7F0E") + 
  geom_point(data=subset(FireMetrics_b, IgCl == "Lightning_Wildlands"),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="gray75") + 
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Human_WUI" | 
                            IgCl == "Lightning_WUI" | 
                            IgCl == "Human_VLD" |
                            IgCl == "Lightning_VLD"),
              method = "glm", se = FALSE, size=0.75) + 
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Human_Wildlands" ),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("#FF7F0E", 0.5)) +
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Lightning_Wildlands"),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("gray75", 0.5)) +
  scale_size_identity() +
  scale_color_manual(values=c("red3", "black")) +
  xlab("Fire year") + ylab("Burned area estimate (km2)") +
  theme_pub() +
  theme(plot.title = element_text(hjust = 0, size = 12),
        #axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none"
  ) +
  facet_grid( ~ Class)

FS_p <- FireMetrics_b %>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(FIRE_YEAR, Fseas, colour = IGNITION)) +
  geom_point(data=subset(FireMetrics_b,
                         IgCl == "Lightning_VLD" | 
                           IgCl == "Lightning_WUI"),
             na.rm=TRUE, size = 0.5, alpha = 0.5) + 
  geom_point(data=subset(FireMetrics_b, IgCl == "Human_WUI" | 
                           IgCl == "Human_VLD" |
                           IgCl == "Human_Wildlands"),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="#FF7F0E") + 
  geom_point(data=subset(FireMetrics_b, IgCl == "Lightning_Wildlands"),
             na.rm=TRUE, size = 0.5, alpha = 0.35, color="gray75") + 
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Lightning_VLD" | 
                          IgCl == "Lightning_WUI"),
              method = "glm", se = FALSE, size=0.75) + 
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Human_WUI" | 
                          IgCl == "Human_VLD" |
                          IgCl == "Human_Wildlands"),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("#FF7F0E", 0.5)) +
  stat_smooth(data=subset(FireMetrics_b,
                          IgCl == "Lightning_Wildlands"),
              method = "glm", se = FALSE, size=0.75, alpha = 0.5, color=alpha("gray75", 0.5)) +
  scale_size_identity() +
  scale_color_manual(values=c("black")) +
  xlab("Fire year") + ylab("Fire season length (days)") +
  theme_pub() +
  theme(plot.title = element_text(hjust = 0, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none"
  ) +
  facet_grid( ~ Class)

grid.arrange(FF_p, FPY_p, BAE_p, FS_p, ncol = 2)
g <- arrangeGrob(FF_p, FPY_p, BAE_p, FS_p, ncol = 2) #generates g
ggsave(file = "Timeseries_FF_BAE_FS.png", g, width = 8, height = 8, dpi=1200) #saves g

FireMetrics_p <- wuw_eco_poly %>%
  group_by(FishID_10k, Class, IGNITION, FIRE_YEAR) %>%
  summarize(tot_fire = n(),
            MeanFireSize = mean(fire_size_km2)) %>%
  mutate(FireInterval = c(NA, diff(FIRE_YEAR)),
         FireInterval = ifelse(is.na(FireInterval), 0, FireInterval)) %>%
  filter(FireInterval != 0) %>%
  group_by(FishID_10k, Class, IGNITION, FIRE_YEAR) %>%
  summarise(num_yr = n(),
            tot_fire = sum(tot_fire),
            MeanFireInterval = ifelse(is.na(mean(FireInterval)), 0 , mean(FireInterval)),
            FirepYear = (tot_fire/num_yr)/(max(num_yr)),
            MinRotation = min(FireInterval),
            MaxRotation = max(FireInterval),
            MeanOccurPerYear = (mean(tot_fire)/(mean(FireInterval))),
            ptsz_n = classify_ptsize_breaks(tot_fire),
            MOPY = classify_occpyear(MeanOccurPerYear),
            FRI = classify_returninterval(MeanFireInterval)) %>%
  ungroup()

FireMetrics_ts <- wuw_eco_poly %>%
  group_by(FishID_10k, Class, IGNITION, DISCOVERY_DOY) %>%
  summarize(tot_fire = n())

MFI <- FireMetrics_p %>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = MeanFireInterval)) +
  geom_density(aes(fill=IGNITION), alpha=0.5, adjust = 2, size = 0.005) + 
  scale_size_identity()+
  scale_fill_manual(values=c("red", "black"), 
                    limits= c("Human", "Lightning")) +
  xlab("Mean fire return interval") + ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_blank(),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none"
  ) +
  facet_grid( ~ Class)

MNFPY <- FireMetrics_p %>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = FirepYear)) +
  geom_density(aes(fill=IGNITION), alpha=0.5, adjust = 2, size = 0.005) + 
  scale_size_identity()+
  scale_fill_manual(values=c("red", "black"), 
                    limits= c("Human", "Lightning")) +
  xlab("Mean number of wildfires per year") + ylab("Density") +
  scale_x_log10(labels = comma) +
  #coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none"
  ) +
  facet_grid( ~ Class)

DISDOY <- FireMetrics_ts %>%
  transform(Class = factor(Class, levels= c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = DISCOVERY_DOY)) +
  geom_histogram(aes(fill=IGNITION), alpha=0.5, binwidth = 7, size = 0.005) + 
  scale_size_identity()+
  scale_fill_manual(values=c("Human" = "red", 
                             "Lightning"  = "black")) +
  xlab("Day of year") + ylab("Number of fires") +
  #coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12),
        #axis.text.x = element_blank(),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none"
  ) +
  facet_grid( ~ Class)

grid.arrange(MFI, MNFPY, DISDOY, ncol = 1)
g <- arrangeGrob(MFI, MNFPY, DISDOY, ncol = 1) #generates g
ggsave(file = "Timeseries_MFI_MNFPY_DOY.png", g, width = 4, height = 8, dpi=1200) #saves g

# Fire Regime analysis ----------------------------------------------------
fr_sum <- wuw_eco_poly %>%
  group_by(FishID_25k, Class) %>%
  summarize(tot_fire = n(), 
            sum_totfire = sum(fire_size_km2),
            avg_totfire = mean(fire_size_km2)) %>%
  ungroup()  %>%
  mutate(ptsz_s = classify_fire_size(avg_totfire))

FireReg <- wuw_eco_poly %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, n_fire) %>%
  left_join(., fr_sum, by = c("Class", "FishID_25k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human), 
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_fire-n_Human),
         n_light_den = (tot_fire-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100)

HumanFireRegime <- FireReg %>%
  filter(n_den >= 80) %>%
  group_by(Class) %>%
  summarise(burn_area = sum(sum_totfire),
            tot_fire = sum(Human)) %>%
  ungroup()

LightningFireRegime <- FireReg %>%
  filter(n_den <= 20) %>%
  group_by(Class) %>%
  summarise(burn_area = sum(sum_totfire),
            tot_fire = sum(Lightning)) %>%
  ungroup()


# Aggregate by FishID ***Short IQR*****------------------------------

eco_sum_bae <- wuw_eco_bae %>%
  group_by(FishID_25k, IGNITION, Class, VLD, Wildlands, WUI) %>%
  summarize(t_cnt = n(),
            t_sum = sum(AREA_km2)) %>%
  ungroup()  %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarise(vld2 = sum(VLD),
            wild2 = sum(Wildlands),
            wui2 = sum(WUI),
            f_cnt= sum(t_cnt),
            sum_fire = sum(t_sum)) %>%
  mutate(pct_burn = ifelse(Class == "WUI", (sum_fire/wui2)*100,
                           ifelse(Class == "VLD", (sum_fire/vld2)*100,
                                  ifelse(Class == "Wildlands", (sum_fire/wild2)*100, 0))),
         pct_burn = ifelse(pct_burn >100, 100, pct_burn),
         pct_class = classify_pctbae(pct_burn),
         ptsz_n = classify_ptsize_breaks(f_cnt)) %>%
  select(FishID_25k, IGNITION, Class, f_cnt, sum_fire, pct_burn, pct_class, ptsz_n)

Eco_IQR <- wuw_eco_bae %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarize(iqr = IQR(DISCOVERY_DOY)) %>%
  ungroup() %>%
  mutate(iqr = ifelse(iqr == 0, 1, 
                      ifelse(iqr == is.na(iqr), 1, iqr))) %>%
  left_join(., eco_sum_bae, by = c("Class", "FishID_25k", "IGNITION"))

conus_iqr <- left_join(fs25_df, Eco_IQR, by = "FishID_25k")

conus_iqr <-conus_iqr %>%
  filter(Class != "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) 

# now create the map
colourCount_iqr = length(unique(bucket(conus_iqr$iqr, 50)))
colourCount_iqr = length(unique(bucket(conus_iqr$iqr, 50)))
getPalette_iqr = colorRampPalette(c("royalblue4", "lightblue1", "lightpink1","red"),
                               space = "Lab")

ManReds = brewer.pal(n = 9, "Reds")[3:10] #there are 9, I exluded the two lighter hues

colourCount_iqrRCB = length(unique(bucket(conus_iqr$iqr, 50)))
getPalette_iqrRCB = colorRampPalette(brewer.pal(9, "Reds")[3:10])

p100 <- conus_iqr %>%
  filter(iqr >= 5) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_iqrRCB(colourCount_iqrRCB), name="Num of Fire\n (in thousands)") + 
  scale_size_discrete(range = c(.3,1), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p101 <- conus_iqr %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .05)+
  geom_point(data = subset(conus_iqr, iqr < 5), aes(colour = factor(buckets), size = ptsz_n), stroke = 0) +
  scale_colour_manual(values = "black", name="") + 
  coord_equal() + 
  scale_size_discrete(range = c(.3,1), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p101a <- p101 + theme(legend.position="none")

ggsave(file = "Gridded_IQR_PctBArea.png", p100a, width = 4, height = 5, dpi=1200) #saves g
ggsave(file = "Gridded_IQR_PctBArea_less5.png", p101a, width = 4, height = 5, dpi=1200) #saves g


# Aggregate by FishID ***Short fire frequency and burned area*****------------------------------

eco_sum <- wuw_eco_poly %>%
  group_by(FishID_25k, Class) %>%
  summarize(tot_fire = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire))

eco_sum2 <- wuw_eco_poly %>%
  group_by(FishID_25k, Class) %>%
  summarize(sum_totfire = sum(fire_size_km2),
            avg_totfire = mean(fire_size_km2),
            pct95th_tot = quantile(fire_size_km2, probs = 0.90, na.rm = TRUE)) %>%
  ungroup()  %>%
  mutate(ptsz_s = classify_fire_size(pct95th_tot))

Eco_Fden <- wuw_eco_poly %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, n_fire) %>%
  left_join(., eco_sum, by = c("Class", "FishID_25k")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human), 
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_fire-n_Human),
         n_light_den = (tot_fire-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100)

Eco_Fsum <- wuw_eco_poly %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarize(pct95th = quantile(fire_size_km2, probs = 0.90, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(IGNITION, pct95th) %>%
  left_join(., eco_sum2, by = c("Class", "FishID_25k")) %>%
  mutate(s_Human = ifelse(is.na(Human), 0, Human), 
         s_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         s_den = (1-(s_Lightning/(s_Human+s_Lightning)))*100)

conus_ff <- left_join(Eco_Fden, Eco_Fsum, by = c("Class", "FishID_25k")) %>%
  left_join(fs25_df, Eco_Fden, by = "FishID_25k")

# now create the map
colourCount_ff = length(unique(bucket(conus_ff$s_den, 10)))
getPalette_ff = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")

p100 <- conus_ff %>%
  filter(n_den != "NA") %>%
  filter(Class != "VLD") %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_ff(colourCount_ff), name="Percent") + 
  scale_size_discrete(range = c(.3,.9), name="Fire size (km2)") +
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
legend <- g_legend(p100) 
ggsave(file = "./legend/Gridded_PctFDen_Legend.png", legend, width = 2, height = 5, dpi=1200) #saves g

p101 <- conus_ff %>%
  filter(s_den != "NA") %>%
  filter(Class != "VLD") %>%
  filter(s_den >= 1) %>%
  mutate(buckets = bucket(s_den, 10)) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour=factor(buckets), size = ptsz_s), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_ff(colourCount_ff), name="% Human") + 
  scale_size_discrete(range = c(.3,.9), name="Fire size (km2)") +
  theme_nothing(legend = TRUE) +
  ggtitle('(B) 95th percentile fire size') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p101a <- p101 + theme(legend.position="none")
legend <- g_legend(p101) 
ggsave(file = "./legend/Gridded_PctFDenSize_Legend.png", legend, width = 2, height = 5, dpi=1200) #saves g

grid.arrange(p100a, p101a, ncol = 2)
g <- arrangeGrob(p100a, p101a, ncol = 2) #generates g
ggsave(file = "Gridded_PctFDen_PctBArea.png", g, width = 8, height = 5, dpi=1200) #saves g

# Seasonal aggregate by FishID ***Short fire frequency and burned area*****------------------------------
eco_sum <- wuw_eco_poly %>%
  filter(Seasons == "Summer" | Seasons == "Spring") %>%
  group_by(FishID_25k, Class, Seasons) %>%
  summarize(tot_fire = n()) %>%
  ungroup() %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire))

Eco_Fden_seasons <- wuw_eco_poly %>%
  filter(Seasons == "Summer" | Seasons == "Spring") %>%
  group_by(FishID_25k, IGNITION, Class, Seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(IGNITION, n_fire) %>%
  left_join(., eco_sum, by = c("Class", "FishID_25k", "Seasons")) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human), 
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = (tot_fire-n_Human),
         n_light_den = (tot_fire-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100) 

Maxseasons <- wuw_eco_poly %>%
  group_by(FishID_25k, IGNITION, Class, Seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(Seasons, n_fire) %>%
  group_by(FishID_25k, Class, IGNITION) %>%
  mutate(Fall = ifelse(is.na(as.numeric(Fall)), 0, as.numeric(Fall)),
         Spring = ifelse(is.na(as.numeric(Spring)), 0, as.numeric(Spring)),
         Summer = ifelse(is.na(as.numeric(Summer)), 0, as.numeric(Summer)),
         Winter = ifelse(is.na(as.numeric(Winter)), 0, as.numeric(Winter)),
         tot_fire = Fall + Spring + Summer + Winter) %>%
  do(get_month_max(.)) 

Full_Maxseasons <- wuw_eco_poly %>%
  group_by(FishID_25k, IGNITION, Class, Seasons) %>%
  summarize(n_fire = n()) %>%
  ungroup() %>%
  spread(Seasons, n_fire) %>%
  group_by(FishID_25k, Class, IGNITION) %>%
  mutate(Fall = ifelse(is.na(as.numeric(Fall)), 0, as.numeric(Fall)),
         Spring = ifelse(is.na(as.numeric(Spring)), 0, as.numeric(Spring)),
         Summer = ifelse(is.na(as.numeric(Summer)), 0, as.numeric(Summer)),
         Winter = ifelse(is.na(as.numeric(Winter)), 0, as.numeric(Winter)),
         tot_fire = Fall + Spring + Summer + Winter) %>%
  left_join(., Maxseasons, by = c("FishID_25k", "Class", "IGNITION")) %>%
  mutate(ptsz_n = classify_ptsize_breaks(tot_fire),
         max_season = as.factor(max_season))

GA_CONUS_maxseason <- left_join(fs25_df, Full_Maxseasons, by = "FishID_25k")

GA_CONUS_season <- left_join(fs25_df, Eco_Fden_seasons, by = "FishID_25k")

colourCount_season = length(unique(bucket(GA_CONUS_season$n_den, 10)))
getPalette_season = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")

p100 <- GA_CONUS_season %>%
  filter(Seasons == "Spring") %>%
  filter(n_den != "NA") %>%
  filter(Class != "VLD") %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets),  size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_season(colourCount_season), name="Percent") + 
  scale_size_discrete(range = c(0.3,.9)) +
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Spring') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Seasons, switch = "y")

p101 <- GA_CONUS_season %>%
  filter(Seasons == "Summer") %>%
  filter(n_den != "NA") %>%
  filter(Class != "VLD") %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets),  size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_season(colourCount_season), name="Percent") + 
  scale_size_discrete(range = c(0.3,.9)) +
  theme_nothing(legend = TRUE) +
  ggtitle('(B) Summer') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Seasons, switch = "y")

p100a <- p100 + theme(legend.position="none")
p101a <- p101 + theme(legend.position="none")

grid.arrange(p100a, p101a, ncol =2)
g <- arrangeGrob(p100a, p101a, ncol = 2) #generates g
ggsave(file = "Gridded_PctFDen_SpringSummer.png", g, width = 8, height = 5, dpi=1200) #saves g

#For Tableau blue
rgb(31,119,180, maxColorValue = 255)
#For Tableau orange
rgb(255,127,14, maxColorValue = 255)
#For Tableau red
rgb(214,39,40, maxColorValue = 255)
#For Tableau green
rgb(44,160,44, maxColorValue = 255)

p102 <- GA_CONUS_maxseason %>%
  filter(Class != "VLD") %>%
  filter(IGNITION == "Human") %>%
  #transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(max_season), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("Winter" = "#1F77B4", 
                                 "Spring" = "#2CA02C", 
                                 "Summer" =  "#D62728", 
                                 "Fall" = "#FF7F0E"), 
                         name="Max Season") + 
  scale_size_discrete(range = c(0.3,.9)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Human started wildfires') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p103 <- GA_CONUS_maxseason %>%
  filter(Class != "VLD") %>%
  filter(IGNITION == "Lightning") %>%  
  #transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(max_season), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("Winter" = "#1F77B4", 
                                "Spring" = "#2CA02C", 
                                "Summer" =  "#D62728", 
                                "Fall" = "#FF7F0E"), 
                     name="Max Season") + 
  scale_size_discrete(range = c(0.3,.9)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('(B) Lightning started wildfires') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p102a <- p102 + theme(legend.position="none")
p103a <- p103 + theme(legend.position="none")

grid.arrange(p102a, p103a, ncol =2)
g <- arrangeGrob(p102a, p103a, ncol = 2) #generates g
ggsave(file = "Gridded_PctFDen_MaxSeason.png", g, width = 8, height = 5, dpi=1200) #saves g

legend <- g_legend(p103) 
ggsave(file = "./legend/Gridded_PctFDen_MaxSeason_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g


# Aggregate by FishID ***WUI Aggregate total burned vs availablet to burn*****------------------------------

eco_sum_bae <- wuw_eco_bae %>%
  group_by(FishID_25k, Class) %>%
  summarize(sum_fire = sum(AREA_km2),
            avg_fire = mean(AREA_km2)) %>%
  ungroup()  %>%
  mutate(ptsz_s = classify_fire_size(sum_fire))

eco_sum <- wuw_eco_wui %>%
  group_by(FishID_25k, Class) %>%
  summarize(sum_wuiarea = sum(AREA_km2)) %>%
  ungroup()  %>%
  mutate(pct_gridcell = (sum_wuiarea/625)*100) %>%
  left_join(., eco_sum_bae, by = c("Class", "FishID_25k")) %>%
  mutate(percent_burned = (sum_fire/sum_wuiarea)*100,
         ptsz_b = classify_wuiburned(percent_burned))

GA_CONUS_wui <- left_join(fs25_df, eco_sum, by = "FishID_25k")

#For Tableau blue
rgb(31,119,180, maxColorValue = 255)
#For Tableau orange
rgb(255,127,14, maxColorValue = 255)
#For Tableau red
rgb(214,39,40, maxColorValue = 255)
#For Tableau green
rgb(44,160,44, maxColorValue = 255)
#For Tableau yellow
rgb(255,217,74, maxColorValue = 255)
#For Tableau orange 2
rgb(255,128,14, maxColorValue = 255)
#For Tableau purple
rgb(148,103,189, maxColorValue = 255)

# now create the map
colourCount_wui = length(unique(bucket(GA_CONUS_wui$pct_gridcell, 25)))
getPalette_wui = colorRampPalette(c("#FFD94A", "#FF800E", "#D62728", "#9467BD"),
                               space = "Lab")

p100 <- GA_CONUS_wui %>%
  filter(pct_gridcell != "NA") %>%
  filter(Class != "VLD") %>%
  filter(pct_gridcell >= 1) %>%
  mutate(buckets = bucket(pct_gridcell, 25)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ptsz_b = factor(ptsz_b, levels=c("0", "0.01 - 2", "3 - 10", "11 - 20", "21 - 30", "> 30"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = .55), stroke = 0.2) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_wui(colourCount_wui), name="% Class area\n available to burn") + 
  scale_size_identity() +
  theme_nothing(legend = TRUE) +
  ggtitle('') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
ggsave(file = "Gridded_WUI_Available.png", p100a, width = 5, height = 6, dpi=1200) #saves g
legend <- g_legend(p100) 
ggsave(file = "./legend/GGridded_WUI_Available_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g

p100b <- GA_CONUS_wui %>%
  filter(pct_gridcell != "NA") %>%
  filter(Class == "VLD") %>%
  filter(pct_gridcell >= 1) %>%
  mutate(buckets = bucket(pct_gridcell, 25)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ptsz_b = factor(ptsz_b, levels=c("0", "0.01 - 2", "3 - 10", "11 - 20", "21 - 30", "> 30"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = .55), stroke = 0.2) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_wui(colourCount_wui), name="% Class area\n available to burn") + 
  scale_size_identity() +
  theme_nothing(legend = TRUE) +
  ggtitle('') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p100c <- p100b + theme(legend.position="none")
ggsave(file = "Gridded_WUI_Available_VLD.png", p100c, width = 5, height = 7, dpi=1200) #saves g
legend <- g_legend(p100b) 
ggsave(file = "./legend/GGridded_WUI_Available_VLD_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g


p101 <- GA_CONUS_wui %>%
  filter(pct_gridcell != "NA") %>%
  #filter(Class != "VLD") %>%
  filter(pct_gridcell >= 1) %>%
  mutate(buckets = bucket(pct_gridcell, 25)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(ptsz_b = factor(ptsz_b, levels=c("0", "0.01 - 2", "3 - 10", "11 - 20", "21 - 30", "> 30"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_b), stroke = 0.1) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_wui(colourCount_wui), name="% Class area\n available to burn") + 
  scale_size_discrete(range = c(0.2,.6), name="% Class\n are burned") +
  theme_nothing(legend = TRUE) +
  ggtitle('') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p101a <- p101 + theme(legend.position="none")
ggsave(file = "Gridded_WUI_AvailableVsBurned.png", p101a, width = 5, height = 7, dpi=1200) #saves g
legend <- g_legend(p101) 
ggsave(file = "./legend/GGridded_WUI_AvailableVsBurned_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g

# Aggregate by FishID ****BAE****------------------------------


eco_sum_bae <- wuw_eco_bae %>%
  group_by(FishID_25k, IGNITION, Class, VLD, Wildlands, WUI) %>%
  summarize(t_cnt = n(),
            t_sum = sum(AREA_km2)) %>%
  ungroup()  %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarise(vld2 = sum(VLD),
            wild2 = sum(Wildlands),
            wui2 = sum(WUI),
            f_cnt= sum(t_cnt),
            sum_fire = sum(t_sum)) %>%
  mutate(pct_burn = ifelse(Class == "WUI", (sum_fire/wui2)*100,
                           ifelse(Class == "VLD", (sum_fire/vld2)*100,
                                  ifelse(Class == "Wildlands", (sum_fire/wild2)*100, 0))),
         pct_burn = ifelse(pct_burn >100, 100, pct_burn),
         pct_class = classify_pctbae(pct_burn),
         ptsz_n = classify_ptsize_breaks(f_cnt)) %>%
  select(FishID_25k, IGNITION, Class, f_cnt, sum_fire, pct_burn, pct_class, ptsz_n)

Gen_Sum <- wuw_eco_bae %>%
  group_by(FishID_25k, Class) %>%
  summarise(f_cnt= n()) %>%
  mutate(ptsz_n = classify_ptsize_breaks(f_cnt))

bae_Den <- wuw_eco_bae %>%
  group_by(FishID_25k, IGNITION, Class) %>%
  summarize(t_sum = sum(AREA_km2)) %>%
  spread(IGNITION, t_sum) %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human), 
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         tot_fire = (n_Human + n_Lightning),
         n_den = ifelse(is.na((1-(n_Lightning/tot_fire))*100), 0, (1-(n_Lightning/tot_fire))*100)) %>%
  gather(., key = IGNITION, value = bae, -FishID_25k, -Class, -n_Human, -n_Lightning, -tot_fire, -n_den) %>%
  mutate(IGNITION = as.factor(IGNITION)) %>%
  left_join(., eco_sum_bae, by = c("FishID_25k", "Class", "IGNITION")) %>%
  select(FishID_25k, IGNITION, Class, pct_class, tot_fire, n_Human, n_Lightning, n_den) %>%
  left_join(., Gen_Sum, by = c("FishID_25k", "Class"))

Den_bae <- left_join(fs25_df, bae_Den, by = "FishID_25k")
GA_CONUS_bae <- left_join(fs25_df, eco_sum_bae, by = "FishID_25k")

# now create the map
colourCount_bae = length(unique(bucket(Den_bae$n_den, 10)))
getPalette_bae = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")

p100_bae <- Den_bae %>%
  filter(IGNITION == "Human") %>%
  filter(n_den != "NA") %>%
  filter(Class != "VLD") %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_bae(colourCount_bae), name="Percent") + 
  scale_size_discrete(range = c(.3,1.5), name="Class burned (%)") +
  theme_nothing(legend = TRUE) +
  ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

ManReds = brewer.pal(n = 9, "Reds")[3:10] #there are 9, I exluded the two lighter hues
  
p100 <- GA_CONUS_bae %>%
  filter(IGNITION == "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(Class != "VLD") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.3,1.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p101 <- GA_CONUS_bae %>%
  filter(IGNITION != "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(Class != "VLD") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.3,1.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  ggtitle('') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) +
  facet_grid(Class ~ IGNITION, switch = "y")

p100a <- p100 + theme(legend.position="none")
p101a <- p101 + theme(legend.position="none")

grid.arrange(p100a, p101a, ncol =2)
g <- arrangeGrob(p100a, p101a, ncol = 2) #generates g
ggsave(file = "Gridded_WUI_BAE.png", g, width = 8, height = 5, dpi=1200) #saves g
legend <- g_legend(p100) 
ggsave(file = "./legend/Gridded_WUI_BAE_Legend.png", legend, width = 2, height = 4.5, dpi=1200) #saves g


# Aggregate by FishID ****Homes destroyed****------------------------------


# Total fire size (km2)
p102 <- GA_CONUS_ICS %>%
  filter(Ignition == "Human") %>%
  filter(Class != "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(sum_costs >= 1) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_s), stroke = 0) +
  scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
                                 "pink1", "orangered","red2", "red4"),
                      name="Total fire suppression cost ($)") +  
  scale_size_discrete(range = c(0.3,.9), name="Fire size") +
  #ggtitle("(A) Human started fires")+
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p103 <- GA_CONUS_ICS %>%
  filter(Ignition == "Lightning") %>%
  filter(Class != "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(sum_costs >= 1) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_s), stroke = 0) +
  scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
                                 "pink1", "orangered","red2", "red4"),
                      name="Total fire suppression cost ($)") +  
  scale_size_discrete(range = c(0.3,.9), name="Fire size") +
  #ggtitle("(B) Lightning started fires") +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')
p102a <- p102 + theme(legend.position="none")
p103a <- p103 + theme(legend.position="none")
g <- arrangeGrob(p102a, p103a, ncol = 2) #generates g
ggsave(file = "Gridded_WUI_CostsvsFireSize.png", g, width = 6, height = 4, dpi=1200) #saves g
legend <- g_legend(p102) 
ggsave(file = "./legend/Gridded_WUI_CostsvsFireSize_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g

p103 <- GA_CONUS_ICS %>%
  filter(Ignition != "Lightning") %>%
  filter(Class != "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(ptsz_sc != "NA") %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_t), stroke = 0) +
 # scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
 #                                "pink1", "orangered","red2", "red4"),
 #                     name="Total fire suppression cost ($)") +  
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.4, 1), name="Total homes threatened") +
  #ggtitle('(A) Human started wildfires') +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Ignition, switch = "y")

p104 <- GA_CONUS_ICS %>%
  filter(Ignition != "Human") %>%
  filter(Class != "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(sum_costs >= 1) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,000"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_t), stroke = 0) +
  # scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
  #                                "pink1", "orangered","red2", "red4"),
  #                     name="Total fire suppression cost ($)") +  
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.4,1), name="Total homes threatened") +
  #ggtitle('(B) Lightning started wildfires') +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Ignition, switch = "y")

p103a <- p103 + theme(legend.position="none")
p104a <- p104 + theme(legend.position="none")

grid.arrange(p103a, p104a, ncol =2)
g <- arrangeGrob(p103a, p104a, ncol = 2) #generates g
ggsave(file = "Gridded_WUI_CostsvsHomesThreat.png", g, width = 8, height = 5, dpi=1200) #saves g

legend <- g_legend(p104) 
ggsave(file = "./legend/Gridded_WUI_CostsvsHomesThreat_Legend.png", legend, width = 2, height = 3.5, dpi=1200) #saves g





# VLD maps ----------------------------------------------------------------
colourCount2 = length(unique(bucket(GA_CONUS$n_den, 10)))
getPalette2 = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")
colourCount = length(unique(bucket(GA_CONUS$s_den, 10)))
getPalette = colorRampPalette(c("royalblue4", "lightblue1", "pink1", "red"),
                               space = "Lab")
v100a <- GA_CONUS %>%
  filter(n_den != "NA") %>%
  filter(Class == "VLD") %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette2(colourCount2), name="Percent") + 
  scale_size_discrete(range = c(0.2,.8), name="# Fires") +
  theme_nothing(legend = TRUE) +
  ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position="none") +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

v100b <- GA_CONUS %>%
  filter(s_den != "NA") %>%
  filter(Class == "VLD") %>%
  filter(s_den >= 1) %>%
  mutate(buckets = bucket(s_den, 10)) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD","Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long, y=lat, group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour=factor(buckets), size = ptsz_s), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette(colourCount), name="% Human") + 
  scale_size_discrete(range = c(0.2,.8), name="Fire size (km2)") +
  theme_nothing(legend = TRUE) +
  ggtitle('(B) Mean fire size') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position="none") +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')


v101a <- GA_CONUS_bae %>%
  filter(pct_burn != "NA") %>%
  filter(Class == "VLD") %>%
  filter(pct_burn >= 1) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = c("< 1" = "deeppink",
                                 "1 - 5" = "royalblue3", 
                                 "5 - 10" = "royalblue1", 
                                 "10 - 20" = "slategray2",
                                 "20 - 30" = "pink1",
                                 "30 - 40" = "orangered", 
                                 "40 - 50" = "red2",
                                 "> 50" = "red3"),
                      name="Percent") +  
  # scale_size_identity() +
  scale_size_discrete(range = c(0.3,.8), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  ggtitle('(C) Total burned area (km2)') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none
        ") +
  facet_grid(Class ~ IGNITION, switch = "y")


v102a <- GA_FireMetrics %>%
  filter(Class == "VLD") %>%
  transform(FRI = factor(FRI, levels=c("0 - 2", "2 - 5", "5 - 7", "7 - 10", "> 10"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(FRI), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("0 - 2" = "#D62728", 
                                "2 - 5" = "#1F77B4", 
                                "5 - 10" =  "#FF7F0E", 
                                "> 10" = "#2CA02C"),                       
                     name="Fire return interval") +  
  scale_size_discrete(range = c(0.3,.9),
                      name = "Total # Fires") +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('(D) Mean fire return interval') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none
        ") +
  facet_grid(Class ~ IGNITION, switch = "y")

v103a <- GA_FireMetrics %>%
  filter(Class == "VLD") %>%
  transform(MOPY = factor(MOPY, levels=c("0 - 25", "25 - 50", "50 - 75", "> 75"))) %>%
  transform(FRI = factor(FRI, levels=c("0 - 2", "2 - 5", "5 - 7", "7 - 10", "> 10"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .25)+
  geom_point(aes(colour = factor(MOPY), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("0 - 25" = "#1F77B4", 
                                "25 - 50" = "#FF7F0E", 
                                "50 - 75" =  "#2CA02C", 
                                "> 75" = "#D62728"),   
                     name="Mean # Fires / Year") +  
  scale_size_discrete(range = c(0.3,.9),
                      name = "Total # Fires") +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  ggtitle('(E) Mean # fires per years') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")
        ,legend.position = "none
        ") +
  facet_grid(Class ~ IGNITION, switch = "y")

v104a <- GA_CONUS_season %>%
  filter(Seasons == "Summer" | Seasons == "Spring") %>%
  filter(n_den != "NA") %>%
  filter(Class == "VLD") %>%
  filter(n_den >= 1) %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets),  size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette(colourCount), name="Percent") + 
  scale_size_discrete(range = c(0.2,.8)) +
  theme_nothing(legend = TRUE) +
  ggtitle('(F) Seasonal fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position="none") +
  facet_grid(Class ~ Seasons, switch = "y")

v105a <- GA_CONUS_ICS %>%
  filter(Class == "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(sum_costs >= 1) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,000"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_t), stroke = 0) +
  scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
                                 "pink1", "orangered","red2", "red4"), name="# Homes threatened") +
  scale_size_discrete(range = c(0.2,.8)) +
  coord_equal() +
  ggtitle('(G) Suppression costs relative to number of threatened homes') +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position="none") +
  facet_grid(Class ~ Ignition, switch = "y")

v106a <- GA_CONUS_ICS %>%
  filter(Class == "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(sum_costs >= 1) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,000"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_s), stroke = 0) +
  scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
                                 "pink1", "orangered","red2", "red4"),
                      name="Total fire suppression cost ($)") +  
  scale_size_discrete(range = c(0.2,.8), name="# Homes threatened") +
  coord_equal() +
  ggtitle('(H) Suppression costs relative to fire size') +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")
        ,legend.position="none"
  ) +
  facet_grid(Class ~ Ignition, switch = "y")


grid.arrange(v100a, v100b, v101a, v102a, v103a, v104a, v105a, v106a, 
             layout_matrix = rbind(c(1,2),c(3), c(4), c(5), c(6), c(7), c(8)))

g <- arrangeGrob(v100a, v100b, v101a, v102a, v103a, v104a, v105a, v106a, 
                 layout_matrix = rbind(c(1,2),c(3), c(4), c(5), c(6), c(7), c(8)))
ggsave(file = "Gridded_All_VLD.png", g, width = 5, height = 9, dpi=1200) #saves g


grid.arrange(v100a, v100b, ncol =2)
g <- arrangeGrob(v100a, v100b, ncol = 2) #generates g
ggsave(file = "Gridded_PctFDen_PctBArea_VLD.png", g, width = 7, height = 3, dpi=1200) #saves g
ggsave(file = "Gridded_TotBArea_VLD.png", v101a, width = 7, height = 3, dpi=1200) #saves g
ggsave(file = "Gridded_MRI_VLD.png", v102a, width = 7, height = 3, dpi=1200) #saves g
ggsave(file = "Gridded_FPY_VLD.png", v103a, width = 7, height = 3, dpi=1200) #saves g
ggsave(file = "Gridded_SeasonalFreq_VLD.png", v104a, width = 7, height = 3, dpi=1200) #saves g
ggsave(file = "Gridded_CostVsThreat_VLD.png", v105a, width = 7, height = 3, dpi=1200) #saves g
ggsave(file = "Gridded_CostVsSize_VLD.png", v106a, width = 7, height = 3, dpi=1200) #saves g


p100 <- GA_CONUS_ICS_noign %>%
  filter(Class == "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(sum_costs >= 1) %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,000"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_s), stroke = 0) +
  scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
                                 "pink1", "orangered","red2", "red4"),
                      name="Total fire suppression cost ($)") +  
  scale_size_discrete(range = c(0.2,.8), name="Total fire size") +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid( ~ Class, switch = "y")

p100a <- p100 + theme(legend.position="none")
ggsave(file = "Gridded_WUI_CostsvsFireSize_WholeUS_VLD.png", p100a, width = 3, height = 2, dpi=1200) #saves g




