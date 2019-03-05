bu_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))

df <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  filter(ignition == 'Human') %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
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

aov.models <- df %>%
  dplyr::select(class_coarse, fire_size, build_up_count_no_zero_0) %>%
  filter(build_up_count_no_zero_0 != 0) %>%
  mutate(build_up_count_no_zero_0 = log(build_up_count_no_zero_0)) %>%
  split(.$class_coarse) %>%
  map(~ aov(build_up_count_no_zero_0 ~ fire_size, data = .x)) %>%
  map( agricolae::HSD.test, trt = 'fire_size', group=TRUE) %>%
  lapply(., '[', c(5)) %>%
  lapply(., function(x) {
    rownames_to_column(as.data.frame(x), var = "fire_size") %>%
      dplyr::select(fire_size, 
                    build_up_count_no_zero_0 = groups.build_up_count_no_zero_0,
                    groups = groups.groups)}) %>%
  dplyr::bind_rows(., .id = 'class_coarse') 

p1 <- df %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands'))) %>%
  transform(fire_size = factor(fire_size, levels=c('0-100', '100-200', '200-400', '400-1k', '1k-10k', '10k-50k', '> 50k'))) %>%
  ggplot(aes(x = fire_size, y = log(build_up_count_no_zero_0))) +
  geom_boxplot(position = position_dodge(width = 0.85), fill = 'gray') +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('Fire Size (ha)') + ylab('log Average home threatened per fire event') +
  geom_vline(aes(xintercept = 7.5), color = 'black') +
  theme_pub() +
  facet_wrap(~class_coarse) +
  theme(legend.position = 'none')

tt <- ggplot_build(p1)$data[[1]] %>%
  dplyr::select(fill, group, PANEL, ymax_final) %>%
  as_tibble() %>%
  mutate(fire_size = case_when(
    group == 1 ~ '0-100',
    group == 2 ~ '100-200',
    group == 3 ~ '200-400', 
    group == 4 ~ '400-1k',
    group == 5 ~ '1k-10k',
    group == 6 ~ '10k-50k',
    group == 7 ~ '> 50k'),
    class_coarse = case_when(
      PANEL == 1 ~ 'WUI',
      PANEL == 2 ~ 'VLD', 
      PANEL == 3 ~ 'Wildlands')) %>%
  dplyr::select(class_coarse, fire_size, ymax_final) %>%
  left_join(aov.models, ., by = c('fire_size', 'class_coarse')) %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'VLD', 'Wildlands')))

p1 <- p1 + geom_text(aes(x=fire_size, y=1.1 +ymax_final, label=groups), data=tt, fontface = "bold",
               position = position_dodge(width = 0.85))

ggsave(file.path(main_text_figs, "figure3.tiff"), p1, width = 6, height = 3, dpi = 600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))
