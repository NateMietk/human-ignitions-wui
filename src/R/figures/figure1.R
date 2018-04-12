# Overall totals by SIZE AND CLASS AND CAUSE
totals_by_cause_class_sizes <- wui_209 %>% 
  mutate(cause = if_else(cause == "Unk", "Human", cause)) %>%
  group_by(syear, class, cause, class) %>%
  summarise(size = sum(area_km2)) %>%
  as.data.frame(.) 

ics_sums <-   wui_209 %>%
  mutate(cause = if_else(cause == "Unk", "Human", cause)) %>%
  group_by(syear, cause, class) %>%
  summarise(costs = sum(costs)) %>%
  left_join(.,  totals_by_cause_class_sizes, by = c("syear", "cause", "class"))  %>%
  group_by(syear, cause, class) %>%
  summarise(costs = sum(costs),
            size = sum(size))


library(mblm)
# https://stackoverflow.com/questions/15211384/any-simple-way-to-get-regression-prediction-intervals-in-r
# Theil-sen of MTBS data from 1984-2015
mblm_with_class <- ics_sums %>%
  filter(class != 'VLD') %>%
  as.data.frame() %>%
  dplyr::select(-geom.x) %>%
  gather(variable, value, -syear, -cause, -class) %>%
  unite(variable, cause, class, variable, sep = "_") %>% 
  group_by(variable) %>%
  nest() %>%
  mutate(
    model1 = map(data, ~ mblm(value ~ syear, data = .x, repeated = FALSE))) %>% 
  mutate(tidy_mblm = map(model1, broom::tidy)) %>% 
  mutate(p_val = map_dbl(tidy_mblm, ~.$p.value[2])) %>% # captures the yearly p-value
  mutate(intercept_estimate = map_dbl(tidy_mblm, ~.$estimate[1])) %>% # captures the intercept estimate used for creating the abline (1st term)
  mutate(syear_estimate = map_dbl(tidy_mblm, ~.$estimate[2])) %>% # captures the syear estimate used for creating the abline (second term)
  dplyr::select(variable, p_val, intercept_estimate, syear_estimate) %>%
  arrange(desc(variable))

mblm_wout_class <- ics_sums %>%
  dplyr::select(-class) %>%
  as.data.frame() %>%
  dplyr::select(-geom.x) %>%
  group_by(syear, cause) %>%
  summarise(costs = sum(costs),
            size = sum(size)) %>%
  gather(variable, value, -syear, -cause) %>%
  unite(variable, cause, variable, sep = "_") %>% 
  group_by(variable) %>%
  nest() %>%
  mutate(
    model1 = map(data, ~ mblm(value ~ syear, data = .x, repeated = FALSE))) %>% 
  mutate(tidy_mblm = map(model1, broom::tidy)) %>% 
  mutate(p_val = map_dbl(tidy_mblm, ~.$p.value[2])) %>% # captures the yearly p-value
  mutate(intercept_estimate = map_dbl(tidy_mblm, ~.$estimate[1])) %>% # captures the intercept estimate used for creating the abline (1st term)
  mutate(syear_estimate = map_dbl(tidy_mblm, ~.$estimate[2])) %>% # captures the syear estimate used for creating the abline (second term)
  dplyr::select(variable, p_val, intercept_estimate, syear_estimate) 



ics_sums_p <- ics_sums %>%
  #transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = costs, color = cause), size = 2) +
  geom_line(aes(y = costs, color = cause),  size = 0.5, alpha = 0.25) +
  # geom_smooth(aes(y = costs/100000000, color = cause), method="glm", method.args = list(family = "poisson"),
  #             se= FALSE, size = 0.75) +  
  scale_color_manual(values = c("#D62728", "#1F77B4")) +
  scale_y_continuous(breaks = pretty(ics_sums$costs, n = 5)) +
  xlab("Year") + ylab("Estimated fire Suppression Cost \n(in hundreds of millions of dollars; $)") +
  theme_pub()  + 
  scale_x_continuous(breaks = pretty(ics_sums$syear, n = 10)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  facet_wrap(~ class, ncol = 1)


ics_sums_s <- ics_sums %>%
  #transform(class = factor(class, levels=c("WUI", "VLD", "Wildlands"))) %>%
  ggplot(aes(x = syear)) +
  geom_point(aes(y = size, color = cause), size = 2) +
  geom_line(aes(y = size, color = cause),  size = 0.5, alpha = 0.25) +
  # geom_smooth(aes(y = size/1000, color = cause), method="gam", method.args = list(family = "poisson"),
  #             se = FALSE, size = 0.75) +
  scale_color_manual(values = c("#D62728", "#1F77B4")) +
  xlab("Year") + ylab("Burned Area \n(in thousands; km2)") +
  theme_pub()  + 
  scale_x_continuous(breaks = pretty(ics_sums$syear, n = 10)) +
  #scale_y_continuous(breaks = pretty(ics_sums$size, n = 5)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  facet_wrap(~ class, ncol = 1, scales = 'free')

grid.arrange(ics_sums_p, ics_sums_s, ncol =2)
g <- arrangeGrob(ics_sums_p, ics_sums_s, ncol =2)
ggsave(file = "figs/draft/main_text/figure1_wui_impact.pdf", g, width = 8, height = 4, dpi=1200, scale = 3, units = "cm")

