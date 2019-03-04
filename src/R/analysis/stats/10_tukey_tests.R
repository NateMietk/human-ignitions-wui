

aov.models <- pfuel %>%
  select(-Year, -Age) %>%
  gather(variable, value, -sb_age, -Site, -Plot) %>%
  split(.$variable) %>%
  map(~ aov(value ~ sb_age + Site/Plot, data = .x)) %>%
  map(HSD.test, trt = 'sb_age')