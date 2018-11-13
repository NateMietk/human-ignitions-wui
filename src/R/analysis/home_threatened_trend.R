
bu_no_na <- as_tibble(as.data.frame(read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds')))) %>%
  dplyr::select(fpa_id, class, ignition, discovery_year, built_class, build_up_count_no_zero_0) %>%
  gather(key = 'buffer_class', value = 'built_count', -fpa_id, -class, -ignition, -discovery_year, -built_class) %>%
  mutate(buffer_class = case_when(
    buffer_class == 'build_up_count_no_zero_0' ~ 'Fire perimeter',
    TRUE ~ NA_character_)) %>% filter(buffer_class == 'Fire perimeter') %>% 
  mutate(fpa_id = as.factor(fpa_id)) %>%
  left_join(., fpa_wui_df, by = c('fpa_id', 'discovery_year', 'ignition', 'class')) %>%  
  filter(built_class == 'Residential') %>%
  group_by(discovery_year) %>%
  summarise(built_count = sum(built_count, na.rm = TRUE))

homes_threat_mblm <- ungroup(bu_no_na) %>%
  mblm(built_count ~ discovery_year, data = ., repeated = FALSE)

plot(bu_no_na$discovery_year, bu_no_na$built_count)
abline(homes_threat_wui)

pct_change_homes_threat <- as.tibble(homes_threat_mblm$fitted.values) %>%
  summarise(difference = (last(value)/first(value)*100))

pred_homes_threat <- predict(homes_threat_mblm, interval="prediction")
new_df <- cbind(bu_no_na, pred_homes_threat)

new_df %>%
  ggplot(aes(x = discovery_year, y = built_count)) +
  #geom_bar(stat = "identity", color = "black", fill = "#D62728", size = 0.25) +
  geom_point(color = "#D62728", size = 2) +
  # geom_line(color = "#D62728",  size = 0.5, alpha = 0.25) +
  geom_abline(intercept = coef(homes_threat_mblm)[1], slope = coef(homes_threat_mblm)[2], col = 'black', linetype = "dashed", size = 0.75) +
  xlab("Year") + ylab("") +
  theme_pub()  +
  scale_x_continuous(breaks = pretty(bu_no_na$discovery_year, n = 5)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = c(0.1, 0.1))
