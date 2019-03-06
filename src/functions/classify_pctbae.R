classify_pctbae <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x > 0.5 & x < 10, "1 - 10",
         ifelse(x >= 10 & x < 20, "10 - 20",
                ifelse(x >= 20 & x < 30, "20 - 30",
                       ifelse(x >= 30 & x < 40, "30 - 40",
                              ifelse(x >= 40 & x < 50, "40 - 50",
                                     "> 50")))))
  
}
