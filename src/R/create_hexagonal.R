
## Set up a raster "template" to use in rasterize()

ext <- extent(rst_fish_25)
xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
n <- 25
r <- raster(ext)
projection(r) <- CRS("+init=epsg:2163")
res(r)=c(25000,25000)

hex_grid_c <- make_grid(as(fishnet_50k, "Spatial"), type = "hexagonal",
                        cell_area = 50000, clip = TRUE)
plot(fishnet_50k[1], col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid_c, border = "orange", add = TRUE)
box()


rst_fish_25 <- rasterize(as(fishnet_25k, "Spatial"), r, "FishID25k")
plot(rst_fish_25)
