classify_fire_size <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 0.25, "< 10 ha",
         ifelse(x >= 0.25 & x < 4, "10 - 400 ha",
                ifelse(x >= 4 & x < 50, "400 - 5000 ha",
                       ifelse(x >= 50 & x < 250, "5000 - 20000 ha", "> 20000 ha"))))
}
