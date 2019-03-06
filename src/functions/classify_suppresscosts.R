classify_suppresscosts <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(is.na(x), 0,
         ifelse(x == 0, "0",
                ifelse(x >= 0.01 & x < 10000, "0.01 - 10k",
                       ifelse(x >= 10000 & x < 50000, "10k - 50k",
                              ifelse(x >= 50000 & x < 1000000, "50k - 1M",
                                     ifelse(x >= 1000000 & x < 10000000, "1M - 10M",
                                            ifelse(x >= 10000000 & x < 20000000, "10M - 20M",
                                                   ifelse(x >= 20000000 & x < 30000000, "20M - 30M",
                                                          ifelse(x >= 30000000 & x < 40000000, "30M - 40M",
                                                                 "> 40M")))))))))
  
}