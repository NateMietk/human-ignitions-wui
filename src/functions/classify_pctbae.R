classify_pctbae <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 1 & x < 10, "1 - 10",
         ifelse(x >= 10 & x < 30, "10 - 30",
                ifelse(x >= 30 & x < 50, "30 - 50",
                       "> 50")))
  
}