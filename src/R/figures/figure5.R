df <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential') %>%
  mutate(fire_size_ha = fire_size_km2*100,
         fire_size = case_when(
           fire_size_ha >  0 & fire_size_ha < 100 ~ '0-100',
           fire_size_ha >= 100 & fire_size_ha < 200 ~ '100-200',
           fire_size_ha >= 200 & fire_size_ha < 400 ~ '200-400',
           fire_size_ha >= 400 & fire_size_ha < 1000 ~ '400-1k',
           fire_size_ha >= 1000 & fire_size_ha < 10000 ~ '1k-10k',
           fire_size_ha >= 10000 & fire_size_ha < 50000 ~ '10k-50k',
           fire_size_ha >= 50000 ~ '> 50k', TRUE ~ NA_character_ )) 

df1 <- df %>%
  group_by(fire_size, class) %>%
  do(data.frame(rbind(smean.cl.boot(.$build_up_count_no_zero_0, B = 10000)))) %>%
  mutate(avg_bu_threat_per_size = round(Mean, 4),
         lower_95_ci = round(Lower, 4),
         upper_95_ci = round(Upper, 4)) %>%
  dplyr::select(-Mean, -Lower, -Upper)

p1 <- df1 %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  ggplot(aes(x = fire_size, y = log(avg_bu_threat_per_size))) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin = log(lower_95_ci), ymax = log(upper_95_ci), width = 0.25)) +
  xlab('Fire Size (ha)') + ylab('log Average home threatened per fire event') +
  geom_vline(aes(xintercept = 4.5), linetype = "dashed", color  = "black") +
  theme_pub() +
  facet_wrap(~class)

ggsave(file.path(main_text_figs, "figure5.tiff"), p1, width = 5, height = 5, dpi = 600, scale = 3, units = "cm") #saves g

system(paste0("aws s3 sync figs s3://earthlab-natem/human-ignitions-wui/figs"))
