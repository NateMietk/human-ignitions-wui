classify_raw_fire_size <- function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 4, "0-4",
         ifelse(x >= 4 & x < 100, "4-100",
                ifelse(x >= 100 & x < 400, "100-400",
                       ifelse(x >= 400 & x < 1000, "400-1000", '>1000'))))
}