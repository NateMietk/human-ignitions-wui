library(sf)
library(parallel)
library(lwgeom)
library(tidyverse)

p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"   #http://spatialreference.org/ref/sr-org/6903/

demo(nc, ask = FALSE, echo = FALSE) 

pt_buf <- nc  %>%
  st_transform(p4string_ea) %>%
  st_centroid(nc) %>%
  st_buffer(20000)  %>%
  st_as_sf(st_bbox(.)) 

nc_id <- unique(pt_buf$CNTY_ID)

jittery <- foreach (i = nc_id, .combine = rbind) %dopar% {
  
  nc_sub <- st_geometry(subset(pt_buf, pt_buf$CNTY_ID == i))
  iter_n <- rep(1:10)
  threshold <- 0.75*max(iter_n)

  op <- lapply(1:length(iter_n), function(j) st_jitter(nc_sub, 3000))

  x <- do.call(c, op)
  jittery <- st_sf(x) %>%
    st_sf() %>%
    lwgeom::st_make_valid() %>%
    st_intersection() %>%
    filter(n.overlaps >= threshold) %>%
    mutate(id = i) %>%
    group_by(id) %>%
    summarize()
  jittery
}


