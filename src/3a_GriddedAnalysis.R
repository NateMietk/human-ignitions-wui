# Base ggplot theme
 p <- ggplot(aes(x, y)) +
   geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
   coord_equal() + 
   theme_nothing(legend = TRUE) +
   theme(plot.title = element_text(hjust = 0, size = 12),
         strip.background = element_blank(),
         strip.text.x = element_blank(),
         strip.text.y = element_blank(),
         legend.key = element_rect(fill = "white")) +
   facet_wrap(~ Class, ncol=1, strip.position = 'left')

p100 <- Den_FPY %>%
  filter(Class != "VLD") %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(FPY = factor(FPY, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  p +
  geom_point(aes(colour=factor(buckets), size = FPY), stroke = 0) +
  scale_colour_manual(values = getPalette_fpy(colourCount_fpy), name="Percent") + 
  scale_size_discrete(range = c(.3,.9), name="Fire per year")
 
 
# PNG Figure 1  --------------------------------------------------------

p100 <- Den_FPY %>%
  filter(Class != "VLD") %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(FPY = factor(FPY, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = FPY), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_fpy(colourCount_fpy), name="Percent") + 
  scale_size_discrete(range = c(.3,.9), name="Fire per year") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p101 <- GA_CONUS_bae %>%
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
  scale_size_discrete(range = c(.3, 0.9), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p102 <- GA_CONUS_ICS %>%
  filter(cause.x != "Lightning") %>%
  filter(Class.x != "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(ptsz_sc != "NA") %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class.x = factor(Class.x, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_t), stroke = 0) +
  # scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
  #                                "pink1", "orangered","red2", "red4"),
  #                     name="Total fire suppression cost ($)") +  
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.3, 0.9), name="Total homes threatened") +
  #ggtitle('(A) Human started wildfires') +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class.x ~ cause.x, switch = "y")


p101a <- p100 + theme(legend.position="none")
p102a <- p101 + theme(legend.position="none")
p103a <- p102 + theme(legend.position="none")

grid.arrange(p101a, p102a, p103a, ncol =3)
g <- arrangeGrob(p101a, p102a, p103a, ncol =3) #generates g

ggsave(file = "Figure_Combo1.PNG", g, width =12, height = 6, dpi=600, scale = 1) #saves g

# EPS Figure 1  --------------------------------------------------------

p100 <- Den_FPY %>%
  filter(Class != "VLD") %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(FPY = factor(FPY, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(buckets), size = FPY), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_fpy(colourCount_fpy), name="Percent") + 
  scale_size_discrete(range = c(.1,.5), name="Fire per year") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p101 <- GA_CONUS_bae %>%
  filter(IGNITION == "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(Class != "VLD") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(.1,.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p102 <- GA_CONUS_ICS %>%
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
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_t), stroke = 0) +
  # scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
  #                                "pink1", "orangered","red2", "red4"),
  #                     name="Total fire suppression cost ($)") +  
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.1, .5), name="Total homes threatened") +
  #ggtitle('(A) Human started wildfires') +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Ignition, switch = "y")


p101a <- p100 + theme(legend.position="none")
p102a <- p101 + theme(legend.position="none")
p103a <- p102 + theme(legend.position="none")

grid.arrange(p101a, p102a, p103a, ncol =3)
g <- arrangeGrob(p101a, p102a, p103a, ncol =3) #generates g
ggsave(file = "Figure_Combo1.EPS", g, width = 15, height = 7, dpi=600, scale = 1, units = "cm") #saves g

#
# PNG Figure 2 --> No Change ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(Class != "VLD") %>%
  filter(IGNITION == "Human") %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .02)+
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(Class != "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr <= 5) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .02)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values="black", name = "Percent") +  
  scale_size_discrete(range = c(.3,0.9), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Gridded_Combo2_NoChange.png", g, width = 8, height = 5, dpi=1200) #saves g


# EPS Figure 2 --> No Change ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(Class != "VLD") %>%
  filter(IGNITION == "Human") %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='gray', fill = "gray99", size = .002)+
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(Class != "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr <= 5) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray', fill = "gray99", size = .002)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values="black", name = "Percent") +  
  scale_size_discrete(range = c(.1,.5), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Gridded_Combo2_NoChange.EPS", g, width = 10, height = 7, dpi=600, scale = 1, units = "cm") #saves g

# PNG Figure 2 ------------------------------------------------------------

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
  scale_size_discrete(range = c(.3,0.9)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  #ggtitle('(C) Seasonality') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(Class != "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr >= 5) %>%
  #transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  #scale_colour_manual(values = getPalette_iqr(colourCount_iqr), name="Num of Fire\n (in thousands)") + 
  scale_colour_manual(values=ManReds, name = "Percent") +  
  #scale_colour_manual(values = getPalette2(colourCount2), name="Percent") + 
  scale_size_discrete(range = c(.3,0.9), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Gridded_Combo2.png", g, width = 8, height = 5, dpi=1200) #saves g
ggsave(file = "Figure_Combo2.EPS", g, width = 15, height = 10, dpi=600, scale = 1, units = "cm") #saves g

# EPS Figure 2 ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(Class != "VLD") %>%
  filter(IGNITION == "Human") %>%
  #transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), colour='black', fill = "gray99", size = .1)+
  geom_point(aes(colour = factor(max_season), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("Winter" = "#1F77B4", 
                                "Spring" = "#2CA02C", 
                                "Summer" =  "#D62728", 
                                "Fall" = "#FF7F0E"), 
                     name="Max Season") + 
  scale_size_discrete(range = c(.1,.5)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  #ggtitle('(C) Seasonality') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(Class != "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr >= 5) %>%
  #transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  #scale_colour_manual(values = getPalette_iqr(colourCount_iqr), name="Num of Fire\n (in thousands)") + 
  scale_colour_manual(values=ManReds, name = "Percent") +  
  #scale_colour_manual(values = getPalette2(colourCount2), name="Percent") + 
  scale_size_discrete(range = c(.1,.5), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Figure_Combo2.EPS", g, width = 10, height = 7, dpi=600, scale = 1, units = "cm") #saves g




# Lightning - WUI, VLD, Wildlands -----------------------------------------
# PNG Figure S1  --------------------------------------------------------

p101 <- GA_CONUS_bae %>%
  filter(IGNITION != "Human") %>%
  filter(pct_burn != "NA") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(.3, 0.9), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p102 <- GA_CONUS_ICS %>%
  filter(Ignition == "Lightning") %>%
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
  scale_size_discrete(range = c(0.3, 0.9), name="Total homes threatened") +
  #ggtitle('(A) Human started wildfires') +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Ignition, switch = "y")


p102a <- p101 + theme(legend.position="none")
p103a <- p102 + theme(legend.position="none")

grid.arrange(p102a, p103a, ncol =2)
g <- arrangeGrob(p102a, p103a, ncol =2) #generates g

ggsave(file = "Figure_Combo1_Lightning.PNG", g, width =7, height = 6, dpi=600, scale = 1) #saves g

# EPS Figure S1  --------------------------------------------------------

p101 <- GA_CONUS_bae %>%
  filter(IGNITION != "Human") %>%
  filter(pct_burn != "NA") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(.1,.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p102 <- GA_CONUS_ICS %>%
  filter(Ignition == "Lightning") %>%
  filter(sum_costs != "NA") %>%
  filter(ptsz_sc != "NA") %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_t), stroke = 0) +
  # scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
  #                                "pink1", "orangered","red2", "red4"),
  #                     name="Total fire suppression cost ($)") +  
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.1, .5), name="Total homes threatened") +
  #ggtitle('(A) Human started wildfires') +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Ignition, switch = "y")


p102a <- p101 + theme(legend.position="none")
p103a <- p102 + theme(legend.position="none")

grid.arrange(p102a, p103a, ncol =2)
g <- arrangeGrob(p102a, p103a, ncol =2) #generates g
ggsave(file = "Figure_Combo1_Lightning.EPS", g, width = 8, height = 7, dpi=600, scale = 1, units = "cm") #saves g



#
# PNG Figure S2 --> No Change ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(IGNITION != "Human") %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .02)+
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(IGNITION == "Lightning") %>%
  filter(iqr <= 5) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .02)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values="black", name = "Percent") +  
  scale_size_discrete(range = c(.3,0.9), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Gridded_Combo2_NoChange_Lightning.png", g, width = 8, height = 7, dpi=1200) #saves g


# EPS Figure S2 --> No Change ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(IGNITION != "Human") %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='gray', fill = "gray99", size = .002)+
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(IGNITION == "Lightning") %>%
  filter(iqr <= 5) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray', fill = "gray99", size = .002)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values="black", name = "Percent") +  
  scale_size_discrete(range = c(.1,.5), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Gridded_Combo2_NoChange_Lightning.EPS", g, width = , height = 7, dpi=600, scale = 1, units = "cm") #saves g

# PNG Figure S2 ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(IGNITION != "Human") %>%
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
  scale_size_discrete(range = c(.3,0.9)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  #ggtitle('(C) Seasonality') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(IGNITION == "Lightning") %>%
  filter(iqr >= 5) %>%
  #transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  #scale_colour_manual(values = getPalette_iqr(colourCount_iqr), name="Num of Fire\n (in thousands)") + 
  scale_colour_manual(values=ManReds, name = "Percent") +  
  #scale_colour_manual(values = getPalette2(colourCount2), name="Percent") + 
  scale_size_discrete(range = c(.3,0.9), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Figure_Combo2_Lightning.png", g, width = 6, height = 5, dpi=1200) #saves g

# EPS Figure S2 ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(IGNITION != "Human") %>%
  #transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), colour='black', fill = "gray99", size = .1)+
  geom_point(aes(colour = factor(max_season), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("Winter" = "#1F77B4", 
                                "Spring" = "#2CA02C", 
                                "Summer" =  "#D62728", 
                                "Fall" = "#FF7F0E"), 
                     name="Max Season") + 
  scale_size_discrete(range = c(.1,.5)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  #ggtitle('(C) Seasonality') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(IGNITION == "Lightning") %>%
  filter(iqr >= 5) %>%
  #transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  #scale_colour_manual(values = getPalette_iqr(colourCount_iqr), name="Num of Fire\n (in thousands)") + 
  scale_colour_manual(values=ManReds, name = "Percent") +  
  #scale_colour_manual(values = getPalette2(colourCount2), name="Percent") + 
  scale_size_discrete(range = c(.1,.5), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Figure_Combo2_Lightning.EPS", g, width = 8, height = 7, dpi=600, scale = 1, units = "cm") #saves g



# Human - VLD ---------------------------------------------------------------------
# PNG Figure S3  --------------------------------------------------------

p100 <- Den_FPY %>%
  filter(Class == "VLD") %>%
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
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p101 <- GA_CONUS_bae %>%
  filter(IGNITION == "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(Class == "VLD") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(.33,1.25), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p102 <- GA_CONUS_ICS %>%
  filter(Ignition != "Lightning") %>%
  filter(Class == "VLD") %>%
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


p101a <- p100 + theme(legend.position="none")
p102a <- p101 + theme(legend.position="none")
p103a <- p102 + theme(legend.position="none")

grid.arrange(p101a, p102a, p103a, ncol =3)
g <- arrangeGrob(p101a, p102a, p103a, ncol =3) #generates g

ggsave(file = "Figure_Combo1_VLD.PNG", g, width =12, height = 3, dpi=600, scale = 1) #saves g

# EPS Figure S3  --------------------------------------------------------

p100 <- Den_FPY %>%
  filter(Class == "VLD") %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(FPY = factor(FPY, levels=c("0 - 2", "2 - 5", "5 - 10", "> 10"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(buckets), size = FPY), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values = getPalette_fpy(colourCount_fpy), name="Percent") + 
  scale_size_discrete(range = c(.1,.5), name="Fire per year") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~ Class, ncol=1, strip.position = 'left')

p101 <- GA_CONUS_bae %>%
  filter(IGNITION == "Human") %>%
  filter(pct_burn != "NA") %>%
  filter(Class == "VLD") %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(pct_class), size = ptsz_n), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(.1,.5), name = "# Fires") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(B) Burned area') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch = "y")

p102 <- GA_CONUS_ICS %>%
  filter(Ignition != "Lightning") %>%
  filter(Class == "VLD") %>%
  filter(sum_costs != "NA") %>%
  filter(ptsz_sc != "NA") %>%
  transform(ptsz_sc = factor(ptsz_sc, levels=c("0.01 - 10,000","10,000 - 50,000", "50,000 - 1,000,000",
                                               "1,000,000 - 10,000,000","10,000,000 - 20,000,000","30,000,000 - 40,000,000","> 40,000,001"))) %>%
  transform(ptsz_s = factor(ptsz_s, levels=c("0 - 2", "3 - 10", "11 - 100", "> 100"))) %>%
  transform(ptsz_t = factor(ptsz_t, levels=c("0 - 100", "100 - 500", "500 - 1000","> 1000"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1) +
  geom_point(aes(colour = as.factor(ptsz_sc), size = ptsz_t), stroke = 0) +
  # scale_colour_manual(values = c("navyblue", "royalblue1", "slategray2",
  #                                "pink1", "orangered","red2", "red4"),
  #                     name="Total fire suppression cost ($)") +  
  scale_colour_manual(values=ManReds, name = "Percent") +  
  scale_size_discrete(range = c(0.1, .5), name="Total homes threatened") +
  #ggtitle('(A) Human started wildfires') +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ Ignition, switch = "y")


p101a <- p100 + theme(legend.position="none")
p102a <- p101 + theme(legend.position="none")
p103a <- p102 + theme(legend.position="none")

grid.arrange(p101a, p102a, p103a, ncol =3)
g <- arrangeGrob(p101a, p102a, p103a, ncol =3) #generates g
ggsave(file = "Figure_Combo1_VLD.EPS", g, width = 15, height = 3.5, dpi=600, scale = 1, units = "cm") #saves g



#
# PNG Figure S4 --> No Change ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(Class == "VLD") %>%
  filter(IGNITION == "Human") %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='black', fill = "gray99", size = .02)+
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(Class == "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr <= 5) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .02)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values="black", name = "Percent") +  
  scale_size_discrete(range = c(.3,0.9), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Gridded_Combo2_NoChange_VLD.png", g, width = 8, height = 2.5, dpi=1200) #saves g


# EPS Figure S4 --> No Change ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(Class == "VLD") %>%
  filter(IGNITION == "Human") %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), 
               colour='gray', fill = "gray99", size = .002)+
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_grid(Class ~ IGNITION, switch ="y")

p100 <- conus_iqr %>%
  filter(Class == "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr <= 5) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='gray', fill = "gray99", size = .002)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  scale_colour_manual(values="black", name = "Percent") +  
  scale_size_discrete(range = c(.1,.5), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white")) +
  facet_wrap( ~ Class, ncol=1, strip.position = 'left')

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Gridded_Combo2_NoChange_VLD.EPS", g, width = 10, height = 3.5, dpi=600, scale = 1, units = "cm") #saves g

# PNG Figure S4 ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(Class == "VLD") %>%
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
  scale_size_discrete(range = c(.3,0.9)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  #ggtitle('(C) Seasonality') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"))

p100 <- conus_iqr %>%
  filter(Class == "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr >= 5) %>%
  #transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .25)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  #scale_colour_manual(values = getPalette_iqr(colourCount_iqr), name="Num of Fire\n (in thousands)") + 
  scale_colour_manual(values=ManReds, name = "Percent") +  
  #scale_colour_manual(values = getPalette2(colourCount2), name="Percent") + 
  scale_size_discrete(range = c(.3,0.9), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"))

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Figure_Combo2_VLD.png", g, width = 8, height = 2.5, dpi=1200) #saves g

# EPS Figure S4 ------------------------------------------------------------

p102 <- GA_CONUS_maxseason %>%
  filter(Class == "VLD") %>%
  filter(IGNITION == "Human") %>%
  #transform(max_season = factor(max_season, levels=c("Winter", "Spring", "Summer", "Fall"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), colour='black', fill = "gray99", size = .1)+
  geom_point(aes(colour = factor(max_season), size = ptsz_n), stroke = 0) +
  scale_color_manual(values = c("Winter" = "#1F77B4", 
                                "Spring" = "#2CA02C", 
                                "Summer" =  "#D62728", 
                                "Fall" = "#FF7F0E"), 
                     name="Max Season") + 
  scale_size_discrete(range = c(.1,.5)) +
  coord_equal() + 
  theme_nothing(legend = TRUE) +
  #ggtitle('(C) Seasonality') +
  theme(plot.title = element_text(hjust = 0, size = 10),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"))

p100 <- conus_iqr %>%
  filter(Class == "VLD") %>%
  filter(IGNITION != "Lightning") %>%
  filter(iqr >= 5) %>%
  #transform(pct_class = factor(pct_class, levels=c("< 1", "1 - 5", "5 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50",  "> 50"))) %>%
  transform(ptsz_n = factor(ptsz_n, levels=c("1 - 25", "26 - 100", "101 - 300", "301 - 700", "> 700"))) %>%
  mutate(buckets = bucket(iqr, 50)) %>%
  transform(Class = factor(Class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x, y)) +
  geom_polygon(data=states, aes(x=long,y=lat,group=group), color='black', fill = "gray99", size = .1)+
  geom_point(aes(colour=factor(buckets), size = ptsz_n), stroke = 0) +
  coord_equal() + 
  #scale_colour_manual(values = getPalette_iqr(colourCount_iqr), name="Num of Fire\n (in thousands)") + 
  scale_colour_manual(values=ManReds, name = "Percent") +  
  #scale_colour_manual(values = getPalette2(colourCount2), name="Percent") + 
  scale_size_discrete(range = c(.1,.5), name="% Class burned") +
  theme_nothing(legend = TRUE) +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"))

p100a <- p100 + theme(legend.position="none")
p102a <- p102 + theme(legend.position="none")

g <- arrangeGrob(p102a, p100a, ncol = 2) #generates g
ggsave(file = "Figure_Combo2_VLD.EPS", g, width = 10, height = 3.5, dpi=600, scale = 1, units = "cm") #saves g
