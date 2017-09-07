# Plot Yearly Costs -------------------------------------------------------

Yearly_Costs <- ICS %>% mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Year, Class, Ignition) %>%
  summarise(spc = sum(SuppressCo)) %>%
  ungroup() %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

Yearly_Costs2 <- ICS %>% mutate(Class = classify_new_categories(WUICLASS10)) %>%
  group_by(Year, Ignition) %>%
  summarise(spc = sum(SuppressCo)) %>%
  ungroup() %>%
  spread(Ignition, spc)

pa <- Yearly_Costs %>%
  filter(Class == "WUI") %>%
  ggplot(aes(x = Year, y = spc, color = Ignition)) +
  geom_smooth(method = "glm", se =FALSE ,fullrange = TRUE, size = 0.75, show.legend = FALSE) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(limits = c(0, 400000000),labels = comma) + 
  xlab("") + ylab("Total fire suppression costs ($)") +
  theme_pub() +
  ggtitle("WUI")

pb <- Yearly_Costs %>%
  filter(Class == "VLD") %>%
  ggplot(aes(x = Year, y = spc, color = Ignition)) +
  geom_smooth(method = "glm", se =FALSE ,fullrange = TRUE, size = 0.75, show.legend = FALSE) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(limits = c(0, 400000000),labels = comma) + 
  xlab("") + ylab("") +
  theme_pub() +
  ggtitle("VLD")

pc <- Yearly_Costs %>%
  filter(Class == "Wildlands") %>%
  ggplot(aes(x = Year, y = spc, color = Ignition)) +
  geom_smooth(method = "glm", se =FALSE ,fullrange = TRUE, size = 0.75, show.legend = FALSE) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(limits = c(0, 400000000),labels = comma) + 
  xlab("Year") + ylab("Total fire suppression costs ($)") +
  theme_pub() +
  ggtitle("Wildlands")

p2 <- Yearly_Costs2 %>%
  ggplot(aes(x = Year, y = spc, color = Ignition)) +
  geom_smooth(method = "glm", se =FALSE ,fullrange = TRUE, size = 0.75, show.legend = FALSE) +
  scale_color_manual(values=c("red", "black")) +
  scale_y_continuous(limits = c(0, 400000000),labels = comma) + 
  xlab("Year") + ylab("") +
  theme_pub() +
  ggtitle("Totals")

grid.arrange(pa, pb, pc, p2, ncol = 2)


# Distance calculations/plotting ****Fire Frequency------------------------------------------
setwd("/Users/NateM/Dropbox/Professional/RScripts/Mietkiewicz_et_al_2017_WUI_Ignitions/")
setwd("./results/Distance")

firefreq_p <- fishdis %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Distance from WUI (km)") + ylab("Ignition frequency") +
  theme_pub() +
  facet_wrap(~NA_L1NAME, scales = "free_y", 
             nrow = 2, labeller = label_wrap_gen(10))

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_cnt <- left_join(min_diffs, pred_diffs) %>%
  mutate(NA_L1NAME = sort(unique(fishdis$NA_L1NAME)),
         xpt_cnt = x) %>%
  select(NA_L1NAME, xpt_cnt) %>%
  left_join(., regionselect, by = "NA_L1NAME") %>%
  arrange(desc(Regions), (xpt_cnt)) 

stmean <- fishdis %>%
  group_by(NA_L1NAME, IGNITION) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(IGNITION, fcnt_mean)

# check to see where the min. diffs fall in plot
firefreq_p <- firefreq_p + 
  geom_vline(aes(xintercept = xpt_cnt), data = xpoints_cnt, 
             linetype = "dashed") +
  geom_hline(aes(yintercept = Human), data = stmean,
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = stmean,
             linetype = "dashed", color = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")
ggsave("Distance_FireFreq.png", firefreq_p, width = 8, height = 5, dpi=1200)

ffreq_xpt <- xpoints_cnt %>%
  #ggplot(aes(x = reorder(NA_L1NAME, -xpt_cnt), y = xpt_cnt)) +
  ggplot(aes(x = NA_L1NAME, y = xpt_cnt)) +
  geom_bar(stat="identity", color = "black") + 
  xlab("") + ylab("Distance from the WUI (km)") +
  theme_pub()+
  ggtitle("(A) Fire frequency") +
  geom_hline(aes(yintercept = 2.4), linetype = "dashed")

# Distance calculations/plotting ****Burn Area------------------------------------------

farea_p <- fishdisbae %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_sum, color = IGNITION)) +
  #geom_point( size = 1, alpha = 0.05) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Distance from WUI (km)") + ylab("Burned area (km2)") +
  theme_pub()  +
  facet_wrap(~NA_L1NAME, scales = "free_y", 
             nrow = 2, labeller = label_wrap_gen(10))

pred_diffs <- ggplot_build(farea_p)$data[[1]] %>%
  tbl_df %>%
  select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_farea <- left_join(min_diffs, pred_diffs) %>%
  mutate(NA_L1NAME = sort(unique(fishdis$NA_L1NAME)),
         xpt_area = x) %>%
  select(NA_L1NAME, xpt_area) %>%
  left_join(., regionselect, by = "NA_L1NAME") %>%
  arrange(desc(Regions), (xpt_area)) 

stmean <- fishdisbae %>%
  group_by(NA_L1NAME, IGNITION) %>%
  summarise(fsum_mean = mean(f_sum)) %>%
  spread(IGNITION, fsum_mean)

# check to see where the min. diffs fall in plot
farea_p <- farea_p + 
  geom_vline(aes(xintercept = xpt_area), data = xpoints_farea, 
             linetype = "dashed") +
  geom_hline(aes(yintercept = Human), data = stmean,
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = stmean,
             linetype = "dashed", color = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")
ggsave("Distance_FireArea.png", farea_p, width = 8, height = 5, dpi=1200)

farea_xpt <- xpoints_farea %>%
  #ggplot(aes(x = reorder(NA_L1NAME, -xpt_area), y = xpt_area)) +
  ggplot(aes(x = NA_L1NAME, y = xpt_area)) +
  geom_bar(stat="identity", color = "black") + 
  theme(legend.position="top") +
  xlab("") + ylab("") +
  ggtitle("(B) Burned area (km2)") +
  theme_pub() +
  geom_hline(aes(yintercept = 2.4), linetype = "dashed")


# Distance calculations/plotting ****IQR Fire season length------------------------------------------

fseason_p <- fishdis %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
  geom_smooth(method = "glm", se = FALSE,
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Distance from WUI (km)") + ylab("Fire season length") +
  theme_pub()  +
  facet_wrap(~NA_L1NAME, 
             nrow = 2, labeller = label_wrap_gen(10))

pred_diffs <- ggplot_build(fseason_p)$data[[1]] %>%
  tbl_df %>%
  select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_fseason <- left_join(min_diffs, pred_diffs) %>%
  mutate(NA_L1NAME = sort(unique(fishdis$NA_L1NAME)),
         xpt_season = x) %>%
  select(NA_L1NAME, xpt_season) %>%
  left_join(., regionselect, by = "NA_L1NAME") %>%
  arrange(desc(Regions), (xpt_season))    

stmean <- fishdis %>%
  group_by(NA_L1NAME, IGNITION) %>%
  summarise(flength_mean = mean(fseason_lngth)) %>%
  spread(IGNITION, flength_mean)

# check to see where the min. diffs fall in plot
fseason_p <- fseason_p + 
  geom_vline(aes(xintercept = xpt_season), data = xpoints_fseason, 
             linetype = "dashed") +
  geom_hline(aes(yintercept = Human), data = stmean,
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = stmean,
             linetype = "dashed", color = "black") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")
  
ggsave("Distance_FireSeason.png", fseason_p, width = 8, height = 5, dpi=1200)

fseason_xpt <- xpoints_fseason %>%
  #ggplot(aes(x = reorder(NA_L1NAME, -xpt_season), y = xpt_season)) +
  ggplot(aes(x = NA_L1NAME, y = xpt_season)) +
  geom_bar(stat="identity", color = "black") + 
  theme(legend.position="top") +
  xlab("") + ylab("") +
  theme_pub() +
  ggtitle("(C) Fire season length") +
  geom_hline(aes(yintercept = 2.4), linetype = "dashed")

grid.arrange(ffreq_xpt, farea_xpt, fseason_xpt, 
             widths=c(0.5, 0.5, 0.5), ncol = 3)
g <-arrangeGrob(ffreq_xpt, farea_xpt, fseason_xpt, 
                widths=c(0.5, 0.5, 0.5), ncol = 3)
ggsave("XPt_Distance.png", g, width = 9, height = 5, dpi=1200)

grid.arrange(firefreq_p, farea_p, fseason_p, nrow = 3)


# Distance calculations/plotting ****Fire Frequency-----------Regions-------------------------------
firefreq_p <- fishdis_reg %>%
  #transform(Region = factor(Region, levels=c("West", "Central", "South East", "North East"))) %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Distance from WUI (km)") + ylab("Ignition frequency") +
  theme_pub()  +
  facet_wrap(~Region, 
             nrow = 2, labeller = label_wrap_gen(10))

pred_diffs <- ggplot_build(firefreq_p)$data[[1]] %>%
  tbl_df %>%
  select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_cnt <- left_join(min_diffs, pred_diffs) %>%
  mutate(Region = sort(unique(fishdis_reg$Region)),
         xpt_cnt = x) %>%
  select(Region, xpt_cnt) %>%
  left_join(., fishdis_reg, by = c("Region")) %>%
  group_by(Region) %>%
  summarise(n = n(),
            xpt_cnt = round(first(xpt_cnt),0),
            xpt_lab = as.factor(xpt_cnt)) %>%
  ungroup()

regmean <- fishdis_reg %>%
  group_by(Region, IGNITION) %>%
  summarise(fcnt_mean = mean(f_cnt)) %>%
  spread(IGNITION, fcnt_mean)

# check to see where the min. diffs fall in plot
firefreq_cent <- fishdis_reg %>%
  filter(Region ==  "Central") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "Central"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_cnt, Region == "Central"), 
            aes(label=paste(xpt_lab, "km", sep = " "), x = 14 + xpt_cnt, y=10, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_west <- fishdis_reg %>%
  filter(Region ==  "West") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("") + ylab("Ignition frequency") +
  ggtitle("West") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "West"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "black") +  
  geom_text(data=subset(xpoints_cnt, Region == "West"), 
            aes(label=paste(xpt_lab, "km", sep = " "), x = 14 + xpt_cnt, y=10, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_se <- fishdis_reg %>%
  filter(Region ==  "South East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75, se = FALSE) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("Distance from WUI (km)") + ylab("Ignition frequency") +
  ggtitle("South East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "South East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "black") +  
  geom_text(data=subset(xpoints_cnt, Region == "South East"), 
            aes(label=paste(xpt_lab, "km", sep = " "), x = 14 + xpt_cnt, y=150, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firefreq_ne <- fishdis_reg %>%
  filter(Region ==  "North East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_cnt, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("Distance from WUI (km)") + ylab("") +
  ggtitle("North East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_cnt), data = subset(xpoints_cnt, Region == "North East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "black") +  
  geom_text(data=subset(xpoints_cnt, Region == "North East"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_cnt, y=10, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
g <- arrangeGrob(firefreq_west, firefreq_cent, firefreq_se, firefreq_ne, ncol =2)
ggsave("Distance_FireFreq_Reg.png", g, width = 6, height = 8, dpi=1200)
ggsave("Distance_FireFreq_Reg.EPS", g, width = 6, height = 7, dpi=1200, scale = 2, units = "cm") #saves g

ffreq_xpt <- xpoints_cnt %>%
  transform(Region = factor(Region, levels=c("West", "Central", "South East", "North East"))) %>%
  #ggplot(aes(x = reorder(Region, -xpt_cnt), y = xpt_cnt)) +
  ggplot(aes(x = Region, y = xpt_cnt)) +
  geom_bar(stat="identity", color = "black") + 
  xlab("") + ylab("Distance from the WUI (km)") +
  theme_pub()+
  ggtitle("(A) Fire frequency") +
  geom_hline(aes(yintercept = 2.4), linetype = "dashed")

# Distance calculations/plotting ****Burn Area----------------Regions--------------------------
library(tidyr)
farea_p <- fishdisbae_reg %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_sum, color = IGNITION)) +
  #geom_point( size = 1, alpha = 0.05) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Distance from WUI (km)") + ylab("Burned area (km2)") +
  theme_pub()  +
  facet_wrap(~Region, scales = "free_y", 
             nrow = 2, labeller = label_wrap_gen(10))

pred_diffs <- ggplot_build(farea_p)$data[[1]] %>%
  tbl_df %>%
  select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_farea <- left_join(min_diffs, pred_diffs) %>%
  mutate(Region = sort(unique(fishdisbae_reg$Region)),
         xpt_area = x) %>%
  select(Region, xpt_area)  %>%
  left_join(., fishdis_reg, by = c("Region")) %>%
  group_by(Region) %>%
  summarise(n = n(),
            xpt_area = round(first(xpt_area),0),
            xpt_lab = as.factor(xpt_area)) %>%
  ungroup()

regmean <- fishdisbae_reg %>%
  group_by(Region, IGNITION) %>%
  summarise(fsum_mean = mean(f_sum)) %>%
  spread(IGNITION, fsum_mean)

# check to see where the min. diffs fall in plot
firebae_cent <- fishdisbae_reg %>%
  filter(Region ==  "Central") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_sum, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_area), data = subset(xpoints_farea, Region == "Central"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_farea, Region == "Central"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_area, y=10, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firebae_west <- fishdisbae_reg %>%
  filter(Region ==  "West") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_sum, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("") + ylab("Ignition frequency") +
  ggtitle("West") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_area), data = subset(xpoints_farea, Region == "West"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_farea, Region == "West"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_area, y=85, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firebae_se <- fishdisbae_reg %>%
  filter(Region ==  "South East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_sum, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("Distance from WUI (km)") + ylab("Ignition frequency") +
  ggtitle("South East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_area), data = subset(xpoints_farea, Region == "South East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_farea, Region == "South East"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_area, y=4000000, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

firebae_ne <- fishdisbae_reg %>%
  filter(Region ==  "North East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = f_sum, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("Distance from WUI (km)") + ylab("") +
  ggtitle("North East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_area), data = subset(xpoints_farea, Region == "North East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_farea, Region == "North East"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_area, y=650, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(firebae_west, firebae_cent, firebae_se, firebae_ne, 
             ncol =2, widths = c(0.5, 0.5))
g <- arrangeGrob(firebae_west, firebae_cent, firebae_se, firebae_ne,             
             ncol =2, widths = c(0.5, 0.5))
ggsave("Distance_FireArea_Reg.png", g, width = 6, height = 8, dpi=1200)

farea_xpt <- xpoints_farea %>%
  transform(Region = factor(Region, levels=c("West", "Central", "South East", "North East"))) %>%
  #ggplot(aes(x = reorder(Region, -xpt_area), y = xpt_area)) +
  ggplot(aes(x = Region, y = xpt_area)) +
  geom_bar(stat="identity", color = "black") + 
  theme(legend.position="top") +
  xlab("") + ylab("") +
  ggtitle("(B) Burned area (km2)") +
  theme_pub() +
  geom_hline(aes(yintercept = 2.4), linetype = "dashed")


# Distance calculations/plotting ****IQR Fire season length---Regions---------------------------------------

fseason_p <- fishdis_reg %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              se = FALSE, fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black")) +
  xlab("Distance from WUI (km)") + ylab("Fire season length") +
  theme_pub()  +
  facet_wrap(~Region, 
             nrow = 2, labeller = label_wrap_gen(10))

pred_diffs <- ggplot_build(fseason_p)$data[[1]] %>%
  tbl_df %>%
  select(colour, y, x, PANEL) %>%
  spread(colour, y) %>%
  mutate(line_diff = abs(black - red))

min_diffs <- pred_diffs %>%
  group_by(PANEL) %>%
  summarize(line_diff = min(line_diff))

xpoints_fseason <- left_join(min_diffs, pred_diffs) %>%
  mutate(Region = sort(unique(fishdis_reg$Region)),
         xpt_season = x) %>%
  select(Region, xpt_season)  %>%
  left_join(., fishdis_reg, by = c("Region")) %>%
  group_by(Region) %>%
  summarise(n = n(),
            xpt_season = round(first(xpt_season),0),
            xpt_lab = as.factor(xpt_season)) %>%
  ungroup()


regmean <- fishdis_reg %>%
  group_by(Region, IGNITION) %>%
  summarise(fseason_mean = mean(fseason_lngth)) %>%
  spread(IGNITION, fseason_mean)


# check to see where the min. diffs fall in plot
fseason_cent <- fishdis_reg %>%
  filter(Region ==  "Central") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("") + ylab("") +
  ggtitle("Central") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_season), data = subset(xpoints_fseason, Region == "Central"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "Central"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_fseason, Region == "Central"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_season, y=85, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fseason_west <- fishdis_reg %>%
  filter(Region ==  "West") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("") + ylab("IQR Range") +
  ggtitle("West") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_season), data = subset(xpoints_fseason, Region == "West"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "West"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_fseason, Region == "West"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_season, y=80, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fseason_se <- fishdis_reg %>%
  filter(Region ==  "South East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("Distance from WUI (km)") + ylab("IQR Range") +
  ggtitle("South East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_season), data = subset(xpoints_fseason, Region == "South East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "South East"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_fseason, Region == "South East"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_season, y=300, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

fseason_ne <- fishdis_reg %>%
  filter(Region ==  "North East") %>%
  ggplot(aes(x = (Ave_NEAR_DIST)*0.001, y = fseason_lngth, color = IGNITION)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              fullrange = TRUE, size = 0.75) +
  scale_color_manual(values=c("red", "black", "black")) +
  xlab("Distance from WUI (km)") + ylab("") +
  ggtitle("North East") +
  scale_x_continuous(limits = c(0, 125)) +
  theme_pub()  + 
  geom_vline(aes(xintercept = xpt_season), data = subset(xpoints_fseason, Region == "North East"),
             linetype = "dashed", color  = "gray") +
  geom_hline(aes(yintercept = Human), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = Lightning), data = subset(regmean, Region == "North East"),
             linetype = "dashed", color = "black") +
  geom_text(data=subset(xpoints_fseason, Region == "North East"), 
            aes(label=paste(xpt_lab, "km", sep = " "), 
                x = 14 + xpt_season, y=70, colour="red"), size = 4) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "none")

grid.arrange(fseason_west, fseason_cent, fseason_se, fseason_ne, 
             ncol =2, widths = c(0.5, 0.5))
g <- arrangeGrob(fseason_west, fseason_cent, fseason_se, fseason_ne,             
                 ncol =2, widths = c(0.5, 0.5))
ggsave("Distance_FireSeason_Reg.png", g, width = 6, height = 8, dpi=1200)
ggsave("Distance_FireSeason_Reg.EPS", g, width = 6, height = 7, dpi=1200, scale = 2, units = "cm") #saves g
