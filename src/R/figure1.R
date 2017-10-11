
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
