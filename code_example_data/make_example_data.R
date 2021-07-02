library(tidyverse)
library(sf)
library(raster)
library(maps)


# Load data
# ==============================================================================

# 50-year snow load data with
loads <- read_csv("../data/us_50year_snow_loads.csv") %>%
  mutate(x = LONGITUDE, y = LATITUDE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

# Continental US polygon for country outline
cont_us <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_union()

# Eco-region polygons
# Throws warnings about true assumption that attribute variables are assumed
# to be constant and st_intersection assumes data are planar. This is expected.
eco3 <- sf::read_sf("../data/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp") %>%
  dplyr::select(ECO3 = NA_L3CODE) %>%
  sf::st_transform(eco3, crs = 4326) %>%
  sf::st_make_valid() %>%
  # reduce regions to plygons with data
  sf::st_intersection(sf::st_convex_hull(sf::st_union(loads$geometry))) %>%
  # We don't care too much about small "islands" where there are smaller parts
  # of some eco-regions embeded in other larger eco-regions. We will remove
  # these islands if they are smaller than 100 km^2.
  mutate(AREA = sf::st_area(.)) %>%
  filter(as.numeric(AREA) > 1e8) %>%
  dplyr::select(-AREA) %>%
  # only look at regions within the continental US
  filter(ECO3 %in% sf::st_intersection(., cont_us)$ECO3)



# Prediction grid
# throws a warning about true assumption that lon/lat variables are assumed
# to be planar
grd <- raster::brick("../data/us_elevation_grid") %>%
  raster::aggregate(9) %>%
  raster::rasterToPoints() %>%
  as.data.frame() %>%
  mutate(LONGITUDE = x, LATITUDE = y) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  filter(sf::st_intersects(., cont_us, sparse = FALSE)[,1])


save(loads, cont_us, eco3, grd, file = "code_example_data.RData")
