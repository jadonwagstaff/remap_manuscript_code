library(tidyverse)
library(sf)
library(mgcv)
library(automap)
library(remap)


# Load data
# ==============================================================================
# Download data from https://github.com/jadonwagstaff/remap_manuscript_code/tree/master/code_example
load("code_example_data.RData")

# Warns that st_simplify does not correctly simplify lon/lat data, this 
# doesn't  matter for this problem and gives us a close enough approximation.
eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 0.1)

# Make distance matrix
eco3_dist <- redist(loads, regions = eco3_simp, region_id = ECO3, progress = TRUE)





gm <- remap(loads, 
             regions = eco3_simp, region_id = ECO3,
             buffer = 50, min_n = 150,
             distances = eco3_dist,
             model_function = mgcv::gam,
             formula = log(EVENT50) ~ s(ELEVATION, k = 15) +
               s(LATITUDE, LONGITUDE, bs = 'sos', k = 75),
             family = gaussian,
             progress = TRUE)

summary(gm)



# Kriging model functions
# ==============================================================================
krig <- function(data, fml) {
  data <- data %>%
    sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45") %>%
    sf::as_Spatial()
  
  out <- list(data = data, fml = fml)
  class(out) <- "krig"
  
  return(out)
}

predict.krig <- function(object, data) {
  if (nrow(data) != 0) {
    data <- data %>%
      sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45") %>%
      sf::as_Spatial()
    
    k <- automap::autoKrige(object$fml, input_data = object$data, 
                            new_data = data, model = "Sph", debug.level = 0)
    
    return(k$krige_output$var1.pred)
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

loads_us <- loads %>%
  sf::st_intersection(cont_us)

kg_preds <- exp(predict(kg, loads_us, smooth = 25, progress = TRUE))

all(dplyr::near(kg_preds, loads_us$EVENT50))


# GAM prediction map
gm_preds <- predict(gm, grd, smooth = 25, progress = TRUE)

range(gm_preds)
range(log(loads$EVENT50))

gm_preds[gm_preds > 3.7] <- 3.7
gm_preds[gm_preds < -2.3] <- -2.3


ggplot(cont_us) +
  geom_tile(data = grd %>% dplyr::mutate(EVENT50 = gm_preds), 
            aes(x = LONGITUDE, y = LATITUDE, fill = EVENT50)) +
  geom_sf(fill = NA, color = NA) +
  scale_fill_viridis_c(option = "inferno",
                       breaks = c(-2.3, 0, 2, 3.7),
                       labels = c("-2.3 (~0.1 kPa)", "0 (1 kPa)", 
                                  "2 (~7 kPa)", "3.7 (~40 kPa)"),
                       name = "Log 50-year event") +
  theme_void()
