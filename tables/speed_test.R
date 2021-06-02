library(tidyverse)
library(sf)
library(rester)
library(maps)
library(remap)

# 50-year snow load data with "V" column used for cross validation
set.seed(42)
loads <- read_csv("../data/us_50year_snow_loads.csv") %>%
  mutate(V = sample(1:10, nrow(.), replace = TRUE),
         x = LONGITUDE, y = LATITUDE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

# Continental US polygon for country outline
cont_us <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_union()

# Eco-region polygons
# Throws 2 warnings about true assumption that attribute variables are assumed
# to be constant and st_intersection assumes data are planar. This is expected.
eco3 <- sf::read_sf("../data/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp") %>%
  dplyr::select(ECO3 = NA_L3CODE) %>%
  sf::st_transform(eco3, crs = 4326) %>%
  sf::st_make_valid() %>%
  # reduce regions to plygons with data
  sf::st_intersection(sf::st_convex_hull(sf::st_union(loads$geometry))) %>%
  mutate(AREA = sf::st_area(.)) %>%
  filter(as.numeric(AREA) > 1e8) %>%
  dplyr::select(-AREA) %>%
  # only look at relevant regions
  filter(ECO3 %in% sf::st_intersection(., cont_us)$ECO3)

# Warns that st_simplify does not correctly simplify lon/lat data, this 
# doesn't  matter for this problem and gives us a close enough approximation.
eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 0.1)



# Prediction gid
# ==============================================================================
grd <- raster::brick("../data/us_elevation_grid") %>%
  raster::rasterToPoints() %>%
  as.data.frame() %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

set.seed(42)
grd_sample <- grd %>%
  filter(1:nrow(.) %in% sample(1:nrow(.), 10000))

t2 <- data.frame(
  step = c("None", 
           "Simplify polygons", 
           " + set max_dist to 25 km",
           " + run in parallel on 4 cores"),
  geographic = rep(as.numeric(NA), 4),
  projected = rep(as.numeric(NA), 4)
)



# Time distance matrix
# ==============================================================================
times <- list()

# None
Sys.time()
times[[1]] <- Sys.time()
remap::redist(grd_sample, eco3, ECO3)

# Simplify polygons
Sys.time()
times[[2]] <- Sys.time()
remap::redist(grd_sample, eco3_simp, ECO3)

# + set max_dist to 25 km
Sys.time()
times[[3]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd)), ], 
                eco3_simp, ECO3, max_dist = 25)
}

# + run in parallel on 4 cores
Sys.time()
times[[4]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd)), ], 
                eco3_simp, ECO3, max_dist = 25, cores = 4)
}
Sys.time()
times[[5]] <- Sys.time()

save(times, file = "times.RData")

# Process times into geographic column of Table 2
for (i in 2:length(times)) {
  if (i %in% c(2, 3)) {
    time <- (nrow(grd) / nrow(grd_sample)) * 
      difftime(times[[i]], times[[i-1]], units = "hours")
  } else {
    time <- difftime(times[[i]], times[[i-1]], units = "hours")
  }
  t2[i - 1, 2] <- signif(time, digits = 2)
}







# ==============================================================================
# Project data
# ==============================================================================
eco3_trans <- eco3 %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

eco3_simp_trans <- eco3_simp %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

grd_trans <- grd %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")

grd_sample_trans <- grd_sample %>%
  sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45")





# Time distance matrix
# ==============================================================================
times2 <- list()

# None
Sys.time()
times2[[1]] <- Sys.time()
remap::redist(grd_sample_trans, eco3_trans, ECO3)

# Simplify polygons
Sys.time()
times2[[2]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd_trans[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd_trans)), ], 
                eco3_simp_trans, ECO3)
}

# + set max_dist to 25 km
Sys.time()
times2[[3]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd_trans[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd_trans)), ], 
                eco3_simp_trans, ECO3, max_dist = 25)
}

# + run in parallel on 4 cores
Sys.time()
times2[[4]] <- Sys.time()
for (i in 1:13) {
  print(i)
  remap::redist(grd_trans[((i - 1) * 1000000 + 1):min(i * 1000000, nrow(grd_trans)), ], 
                eco3_simp_trans, ECO3, max_dist = 25, cores = 4)
}
Sys.time()
times2[[5]] <- Sys.time()

save(times2, file = "times2.RData")

# Process times into geographic column of Table 2
for (i in 4:length(times2)) {
  if (i == 2) {
    print((nrow(grd) / nrow(grd_sample)) * difftime(times2[[i]], times2[[i-1]], units = "hours"))
  } else {
    print(difftime(times2[[i]], times2[[i-1]], units = "hours"))
  }
}


# Process times into projected column of Table 2
for (i in 2:length(times2)) {
  if (i %in% c(2, 3)) {
    time <- (nrow(grd) / nrow(grd_sample)) * 
      difftime(times2[[i]], times2[[i-1]], units = "hours")
  } else {
    time <- difftime(times2[[i]], times2[[i-1]], units = "hours")
  }
  t2[i - 1, 3] <- signif(time, digits = 2)
}












