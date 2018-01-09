
# NIFC v Lise v FamWeb ----------------------------------------------------

setwd("/Users/NateM/Dropbox/Professional/RScripts/Mietkiewicz_et_al_2017_WUI_Ignitions/data")
nifc <- read.csv('NIFC_Fam_Lise.csv', header = TRUE, stringsAsFactors = TRUE)

nifc_p <- nifc %>%
  ggplot() +
  geom_point(aes(x = Year, y = NIFC/1000000000), color = "#1F77B4", size = 3) +
  geom_point(aes(x = Year, y = WVW/1000000000), color = "#2CA02C", size = 3) +
  geom_line(aes(x = Year, y = NIFC/1000000000), color = "#1F77B4", alpha = 0.5) +
  geom_line(aes(x = Year, y = WVW/1000000000), color = "#2CA02C", alpha = 0.5) +
  geom_smooth(aes(x = Year, y = NIFC/1000000000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75, color = "#1F77B4") +
  geom_smooth(aes(x = Year, y = WVW/1000000000), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75, color = "#2CA02C") +
  xlab("Year") + ylab("Fire Suppression Cost (in billions of dollars)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right")
ggsave(file = "CostCompare.png", nifc_p, width = 5, height = 4, dpi=1200)


setwd("/Users/nami1114/Dropbox/Professional/RScripts/Mietkiewicz_et_al_2017_WUI_Ignitions/data")
nifc <- read.csv('NIFC_FNum_Area_85-2015.csv', header = TRUE, stringsAsFactors = TRUE)

nifc_p <- nifc %>%
  ggplot(aes(x = Year)) +
  geom_point(aes(y = Spending/1000000000), color = "#1F77B4", size = 2) +
  geom_point(aes(y = (Acres*0.00404686)/10000), color = "#D62728", size = 2) +
  geom_line(aes(y = Spending/1000000000), color = "#1F77B4", size = 0.5, alpha = 0.5) +
  geom_line(aes(y = (Acres*0.00404686)/10000), color = "#D62728", size = 0.5, alpha = 0.5) +
  geom_smooth(aes(y = Spending/1000000000), method="glm", method.args = list(family = "poisson"),
               se= FALSE, size = 0.75, color = "#1F77B4") +
  geom_smooth(aes(y = (Acres*0.00404686)/10000), method="glm", method.args = list(family = "poisson"),
               se= FALSE, size = 0.75, color = "#D62728")+
  xlab("Year") + ylab("Fire Suppression Cost (in billions of dollars; $)") +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Burned area (in tens of thousands; km2)")) +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right")
ggsave(file = "BurnArea_Costs_85_16.pdf", nifc_p, width = 7, height = 6, dpi=1200,
       scale = 2, units = "cm")

ics_sums <- wuw_eco_ICS %>%
  mutate(Size = ifelse(IncidentNu =="CO-PSF-283", 15.6, Size)) %>%
  filter(Year > 2001) %>%
  group_by(Year, Ignition) %>%
  summarise(costs = sum(costs),
            size = sum(Size))

ics_sums_p <- ics_sums %>%
  ggplot(aes(x = Year)) +
  geom_point(aes(y = costs/100000000, color = Ignition), size = 2) +
  geom_line(aes(y = costs/100000000, color = Ignition),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = costs/100000000, color = Ignition), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Year") + ylab("Fire Suppression Cost \n(in hundreds of millions of dollars; $)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") 

ics_sums_s <- ics_sums %>%
  ggplot(aes(x = Year)) +
  geom_point(aes(y = size/10000, color = Ignition), size = 2) +
  geom_line(aes(y = size/10000, color = Ignition),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = size/10000, color = Ignition), method="glm", method.args = list(family = "poisson"),
              se = FALSE, size = 0.75) +
  scale_color_manual(values = c("#1F77B4", "#D62728")) +
  xlab("Year") + ylab("Burned Area \n(in tens of thousands; km2)") +
  theme_pub()  + 
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") 

grid.arrange(ics_sums_p, ics_sums_s, ncol =2)
g <- arrangeGrob(ics_sums_p, ics_sums_s, ncol =2)
ggsave(file = "Ignition_Costs_0213_HL.pdf", g, width = 8, height = 4, dpi=1200, scale = 3, units = "cm")

