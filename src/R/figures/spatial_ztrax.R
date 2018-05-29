library(devtools)
install_github("hadley/rlang", dependencies = TRUE, force = TRUE)
install_github("tidyverse/ggplot2",force=TRUE)
.rs.restartR()


classify_bu <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 25, "0 - 25",
         ifelse(x >= 25 & x < 250, "25 - 250",
                ifelse(x >= 250 & x < 1000, "250 - 1000",
                       ifelse(x >= 1000 & x < 10000, "1000 - 10000",
                              ifelse(x >= 10000 & x < 100000, "10000 - 100000",
                                     "> 100000")))))
}

classify_bu_per_fire <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 10, "0 - 10",
         ifelse(x >= 10 & x < 50, "10 - 50",
                ifelse(x >= 50 & x < 250, "50 - 250",
                       ifelse(x >= 250 & x < 1000, "250 - 500",
                              "> 500"))))
}

fire_density <- bu_cleaned %>%
  group_by(fishid50k, ignition) %>%
  summarise(fcnt = n()) %>%
  ungroup() %>%
  filter(!is.na(ignition)) %>%
  spread(ignition, fcnt) %>%
  left_join(., fishnet_50k, by = "fishid50k") %>%
  as.data.frame() %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = ((n_Human+n_Lightning)-n_Human),
         n_light_den = ((n_Human+n_Lightning)-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100) %>%
  dplyr::select(fishid50k, n_den) %>%
  as_tibble()

bu_df <- bu_cleaned %>%
  dplyr::select(-geom) %>%
  filter(bu > 0) %>%
  group_by(fishid50k, ignition) %>%
  summarise(fcnt = n(),
            bu = sum(bu, na.rm = TRUE),
            rate = bu/(2016-1992),
            bu_per_fire = bu/fcnt) %>%
  mutate(bu_class = classify_bu(bu),
         rate_class = classify_bu(rate),
         bu_per_fire_class = classify_bu_per_fire(bu_per_fire)) %>%
  left_join(fs50_df, ., by = "fishid50k") %>%
  left_join(fire_density, ., by = "fishid50k") %>%
  na.omit()

p1 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(bu_class = factor(bu_class, levels = c("0 - 25", "25 - 250", "250 - 1000", "1000 - 10000", "10000 - 100000", "> 100000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(1, 5), name="Fire size (km2)") +
  theme_map() +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white"))
p2 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(bu_per_fire_class = factor(bu_per_fire_class, levels = c("0 - 10", "10 - 50", "50 - 250", "250 - 500", "> 500"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_per_fire_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(1, 5), name="Homes threatened /nper wildfire") +
  theme_map() +
  # ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white"))

fire_density <- bu_cleaned %>%
  filter(class != 'Other') %>%
  mutate(class =  ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                          ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))) %>%
  group_by(fishid50k, ignition, class) %>%
  summarise(fcnt = n()) %>%
  ungroup() %>%
  filter(!is.na(ignition)) %>%
  spread(ignition, fcnt) %>%
  left_join(., fishnet_50k, by = "fishid50k") %>%
  as.data.frame() %>%
  mutate(n_Human = ifelse(is.na(Human), 0, Human),
         n_Lightning = ifelse(is.na(Lightning), 0, Lightning),
         n_human_den = ((n_Human+n_Lightning)-n_Human),
         n_light_den = ((n_Human+n_Lightning)-n_Lightning),
         n_den = (1-(n_human_den/(n_human_den+n_light_den)))*100) %>%
  dplyr::select(fishid50k, class, n_den) %>%
  as_tibble()

bu_df <- bu_cleaned %>%
  filter(class != 'Other') %>%
  mutate(class =  ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                          ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))) %>%
  dplyr::select(-geom) %>%
  filter(bu > 0) %>%
  group_by(fishid50k, ignition, class) %>%
  summarise(fcnt = n(),
            bu = sum(bu, na.rm = TRUE),
            rate = bu/(2016-1992),
            bu_per_fire = bu/fcnt) %>%
  mutate(bu_class = classify_bu(bu),
         rate_class = classify_bu(rate),
         bu_per_fire_class = classify_bu_per_fire(bu_per_fire)) %>%
  left_join(fs50_df, ., by = "fishid50k") %>%
  left_join(fire_density, ., by = c('class', "fishid50k")) %>%
  na.omit()

p1 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(class = factor(class, levels=c('Urban', "WUI", "VLD", "Wildlands"))) %>%
  transform(bu_class = factor(bu_class, levels = c("0 - 25", "25 - 250", "250 - 1000", "1000 - 10000", "10000 - 100000", "> 100000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(.25, 2), name="Fire size (km2)") +
  theme_map() +
  #ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class)
p2 <- bu_df %>%
  mutate(buckets = bucket(n_den, 10)) %>%
  transform(class = factor(class, levels=c('Urban', "WUI", "VLD", "Wildlands"))) %>%
  transform(bu_per_fire_class = factor(bu_per_fire_class, levels = c("0 - 10", "10 - 50", "50 - 250", "250 - 500", "> 500"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), color='black', fill = "gray99", size = .25) +
  geom_point(aes(x = coords.x1, y = coords.x2,
                 colour = factor(buckets), size = bu_per_fire_class), stroke = 0) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(11, "RdYlBu"))) +
  scale_size_discrete(range = c(.25, 2.5), name="Homes threatened /nper wildfire") +
  theme_map() +
  # ggtitle('(A) Fire frequency') +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background=element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~class)


