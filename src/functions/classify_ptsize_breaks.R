classify_ptsize_breaks <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes
  ifelse(x >= 1 & x <= 100, "1 - 100",
         ifelse(x >= 101 & x <= 300, "101 - 300",
                ifelse(x >= 301 & x <= 700, "301 - 700",
                       "> 700")))
}