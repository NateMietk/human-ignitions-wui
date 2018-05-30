fpa_df <- as.data.frame(fpa_wui) %>%
  setNames(tolower(names(.))) %>%
  group_by(discovery_year, ignition) %>%
  summarise(f_cnt_fpa = n(),
            burned_area = sum(fire_size_km2))

df <- fpa_cause_bu_df %>%
  dplyr::select(ignition, discovery_year, bu) %>%
  left_join(., fpa_df, by = c('ignition', 'discovery_year'))

fpa_p <- df %>%
  ggplot(aes(x =  discovery_year, y = burned_area*0.0001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    level = 0.90,
    size = 1
  ) +
  scale_color_manual(values = c("#b2182b",
                                '#2166ac')) +

  theme_pub() +
  xlab("Year") + ylab("Burned area by wildfire (in 10,000 km2)") +
  # ggtitle('(b) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

fpa_cause_bu_p <- df %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = ignition, color = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    level = 0.90,
    size = 1
  ) +
  theme_pub() +
  xlab("Year") + ylab("Threatened building unit (in 100,000 units)") +
  scale_color_manual(values = c("#b2182b",
                                '#2166ac')) +
  scale_y_continuous(limits = c(0, 6)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

grid.arrange(fpa_p, fpa_cause_bu_p, nrow = 1)

