library(tidyverse)
library(sf)
library(mgcv)
library(automap)
library(gstat)
library(remap)


# Load data
# ==============================================================================
# Download data from https://github.com/jadonwagstaff/remap_manuscript_code/tree/master/code_example
load("code_example_data.RData")

# Simplify polygons
eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 10000) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  sf::st_cast("MULTIPOLYGON")

# Make distance matrix
eco3_dist <- redist(loads, regions = eco3_simp, region_id = ECO3, progress = TRUE)



lmod <- remap(loads,
              regions = eco3_simp, region_id = ECO3,
              buffer = 50, min_n = 150,
              distances = eco3_dist,
              model_function = stats::lm,
              formula = log(EVENT50) ~ ELEVATION,
              progress = TRUE)


gm <- remap(loads,
             regions = eco3_simp, region_id = ECO3,
             buffer = 50, min_n = 150,
             distances = eco3_dist,
             model_function = mgcv::gam,
             formula = log(EVENT50) ~ s(ELEVATION, k = 15) +
               s(LATITUDE, LONGITUDE, bs = 'sos', k = 75),
             family = gaussian,
             progress = TRUE)

gm

head(names(gm$models), 3)

class(gm$models[[1]])

head(gm$regions, 3)




# Kriging model functions
# ==============================================================================
krig <- function(data, fml) {
  data <- data %>%
    sf::st_transform(sf::st_crs("+proj=laea +x_0=0 +y_0=0
                                +lon_0=-100 +lat_0=45")) %>%
    sf::as_Spatial()

  out <- list(data = data, fml = fml)
  class(out) <- "krig"

  return(out)
}

predict.krig <- function(object, data) {
  if (nrow(data) != 0) {
    data <- data %>%
      sf::st_transform(sf::st_crs("+proj=laea +x_0=0 +y_0=0
                                  +lon_0=-100 +lat_0=45")) %>%
      sf::as_Spatial()

    variogram_object <- automap::autofitVariogram(object$fml, object$data,
                                                  model = "Sph")

    k <- gstat::krige(object$fml, object$data,
                      data, variogram_object$var_model,
                      debug.level = 0)

    return(k$var1.pred)
  }
  return(NULL)
}

kg <- remap(loads,
            regions = eco3_simp, region_id = ECO3,
            buffer = 50, min_n = 150,
            distances = eco3_dist,
            model_function = krig,
            fml = log(EVENT50) ~ ELEVATION,
            progress = TRUE)

loads_us <- sf::st_filter(loads, cont_us)

kg_preds <- exp(predict(kg, loads_us, smooth = 25, progress = TRUE))

all(dplyr::near(kg_preds, loads_us$EVENT50))


# Distances and predictions
grd_dist <- redist(grd, regions = eco3_simp, region_id = ECO3,
                   max_dist = 25, progress = TRUE)

gm_preds <- predict(gm, grd, smooth = 25, distances = grd_dist, progress = TRUE)
gm_preds <- exp(gm_preds)

range(gm_preds)
range(loads$EVENT50)

gm_preds[gm_preds > 40.2] <- 40.2
gm_preds[gm_preds < 0.1] <- 0.1


ggplot(cont_us) +
  geom_tile(data = grd %>% dplyr::mutate(EVENT50 = gm_preds),
            aes(x = LONGITUDE, y = LATITUDE, fill = EVENT50)) +
  geom_sf(fill = NA, color = NA) +
  scale_fill_viridis_c(option = "inferno",
                       trans = "log10",
                       breaks = c(0.1, 1, 7, 40),
                       labels = c("0.1 kPa", "1 kPa", "7 kPa", "40 kPa"),
                       name = "50-year event") +
  theme_void()


# Utah snowpack data
utsp2011 <- utapr1 %>%
  dplyr::filter(YEAR == 2011) %>%
  dplyr::mutate(WESD = WESD + 1)

utlmod <- remap(utsp2011,
                regions = utws, region_id = HUC2,
                buffer = 20, min_n = 30,
                model_function = stats::lm,
                formula = log(WESD) ~ ELEVATION,
                progress = TRUE)

utgm <- remap(utsp2011,
              regions = utws, region_id = HUC2,
              buffer = 20, min_n = 30,
              model_function = mgcv::gam,
              formula = log(WESD) ~ s(ELEVATION, k = 5) +
                s(LATITUDE, LONGITUDE, bs = 'sos', k = 20),
              family = gaussian,
              progress = TRUE)

utkg <- remap(utsp2011,
              regions = utws, region_id = HUC2,
              buffer = 20, min_n = 30,
              model_function = krig,
              fml = log(WESD) ~ ELEVATION,
              progress = TRUE)



