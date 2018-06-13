bu_cleaned <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui),
            by = "fpa_id") %>%
  filter(fire_size_km2 > 0.00025)


bu_wui_cleaned <- sum_wui_bu %>%
  setNames(tolower(names(.))) %>%
  gather(variable, bu, -id_sp, -blk10) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui),
            by = "blk10") %>%
  filter(fire_size_km2 > 0.00025)