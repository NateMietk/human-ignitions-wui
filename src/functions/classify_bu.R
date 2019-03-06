classify_bu <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 25 & x < 250, "0 - 250",
         ifelse(x >= 250 & x < 1000, "250 - 1000",
                ifelse(x >= 1000 & x < 10000, "1000 - 10000",
                       "> 10000")))
}