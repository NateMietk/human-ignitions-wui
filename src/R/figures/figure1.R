# Overall totals by SIZE AND CLASS AND CAUSE
totals_by_cause_class_sizes <- wui_209 %>% 
  mutate(cause = if_else(cause == "Unk", "Human", cause)) %>%
  group_by(syear, class, cause) %>%
  summarise(size = sum(area_km2)) %>%
  as.data.frame(.) 

ics_sums <-   wui_209 %>%
  mutate(cause = if_else(cause == "Unk", "Human", cause)) %>%
  group_by(syear, cause, class) %>%
  summarise(costs = sum(costs)) %>%
  left_join(.,  totals_by_cause_class_sizes, by = c("syear", "cause", "class"))  %>%
  group_by(syear, cause) %>%
  summarise(costs = sum(costs),
            size = sum(size))

ics_sums_p <- ics_sums %>%
  #transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = costs/100000000, color = cause), size = 2) +
  geom_line(aes(y = costs/100000000, color = cause),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = costs/100000000, color = cause), method="glm", method.args = list(family = "poisson"),
              se= FALSE, size = 0.75) +
  scale_color_manual(values = c("#D62728", "#1F77B4")) +
  scale_y_continuous(breaks = pretty(ics_sums$costs/100000000, n = 5)) +
  xlab("Year") + ylab("Estimated fire Suppression Cost \n(in hundreds of millions of dollars; $)") +
  theme_pub()  + 
  scale_x_continuous(breaks = pretty(ics_sums$syear, n = 10)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

ics_sums_s <- ics_sums %>%
  #transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = size/1000, color = cause), size = 2) +
  geom_line(aes(y = size/1000, color = cause),  size = 0.5, alpha = 0.25) +
  geom_smooth(aes(y = size/1000, color = cause), method="gam", method.args = list(family = "poisson"),
              se = FALSE, size = 0.75) +
  scale_color_manual(values = c("#D62728", "#1F77B4")) +
  xlab("Year") + ylab("Burned Area \n(in thousands; km2)") +
  theme_pub()  + 
  scale_x_continuous(breaks = pretty(ics_sums$syear, n = 10)) +
  scale_y_continuous(breaks = pretty(ics_sums$size/1000, n = 5)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none")

grid.arrange(ics_sums_p, ics_sums_s, ncol =1)
g <- arrangeGrob(ics_sums_p, ics_sums_s, ncol =2)
ggsave(file = "results/Ignition_Costs_0213_HL.pdf", g, width = 8, height = 4, dpi=1200, scale = 2.5, units = "cm")

