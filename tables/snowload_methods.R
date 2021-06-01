library(tidyverse)
library(mgcv)


# Load data
# ==============================================================================

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

# Make distance matrix
eco3_dist <- remap::redist(loads, regions = eco3_simp, region_id = ECO3)



# General model building and evaluation functions
# ==============================================================================

# Cross validation function for eco-region remap model
# Parameter - modeling function
# Returns - cross validated MSE
regional_cv <- function(fun, ...) {
  preds <- rep(as.numeric(NA), nrow(loads))
  
  for (k in 1:max(as.numeric(loads$V))) {
    pred_index <- loads$V == k
    
    models <- remap::remap(loads[!pred_index, ],
                           regions = eco3_simp, region_id = ECO3,
                           model_function = fun,
                           buffer = 50, min_n = 150,
                           distances = eco3_dist[!pred_index, ],
                           ...)
    
    preds[pred_index] <- predict(models,
                                 loads[pred_index, ],
                                 smooth = 25,
                                 distances = eco3_dist[pred_index, ])
  }
  
  # Return MSE
  c(MSE = mean((preds - log(loads$EVENT50))^2))
}

# Cross validation function for national level snow load model
# Parameter - modeling function
# Returns - cross validated MSE
national_cv <- function(fun, ...) {
  preds <- rep(as.numeric(NA), nrow(loads))
  
  for (k in 1:max(as.numeric(loads$V))) {
    pred_index <- loads$V == k
    
    model <- fun(loads[!pred_index, ], ...)
    
    preds[pred_index] <- predict(model, loads[pred_index, ])
  }
  
  # Return MSE
  c(MSE = mean((preds - log(loads$EVENT50))^2))
}


# Kriging model functions
# ==============================================================================
krig <- function(data) {
  fml <- log(EVENT50) ~ ELEVATION
  data <- data %>%
    sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45") %>%
    sf::as_Spatial()
  
  # This is to avoid warnings from sp::proj4string which are letting the 
  # user know that proj4string is outdated. This doesn't affect the output
  raster::crs(data) <- NA
  
  out <- list(data = data, fml = fml)
  class(out) <- "krig"
  
  return(out)
}

predict.krig <- function(object, data) {
  if (nrow(data) != 0) {
    data <- data %>%
      sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45") %>%
      sf::as_Spatial()
    
    # This is to avoid warnings from sp::proj4string which are letting the 
    # user know that proj4string is outdated. This doesn't affect the output
    raster::crs(data) <- NA
    
    k <- automap::autoKrige(object$fml, input_data = object$data, 
                            new_data = data, model = "Sph", debug.level = 0)
    
    return(k$krige_output$var1.pred)
  }
  return(NULL)
}



# PRISM functions
# ==============================================================================
prism_function <- function(data) {
  # just save the forumula and data
  fml <- log(EVENT50) ~ ELEVATION
  
  data <- sf::as_Spatial(data)
  
  out <- list(data = data, fml = fml)
  class(out) <- "prism"
  
  return(out)
}

predict.prism <- function(object, data) {
  if (nrow(data) != 0) {
    data <- sf::as_Spatial(data)
    
    # Suppressing warnings from sp::proj4string which are just letting the user
    # know that proj4string is outdated. This doesn't affect the output
    pkgcond::suppress_warnings(
      out <- snowload::prism(formula = object$fml, 
                             newdata = data, 
                             locations = object$data),
      pattern = "CRS object has comment, which is lost in output"
    )
  } else return(NULL)
}



# Do cross validation for different modeling approaches
# ==============================================================================
cv <- data.frame(
  Model = rep("", 4),
  National = rep(as.numeric(NA), 4),
  Regional = rep(as.numeric(NA), 4),
  stringsAsFactors = FALSE
)

#c("GAM", "OLS", "Kriging", "Prism", "IDW")
# GAM
cv[1, 1] <- "GAM"
cv[1, 2] <- national_cv(mgcv::gam, 
                      formula = log(EVENT50) ~ s(ELEVATION, k = 50) + 
                        s(LATITUDE, LONGITUDE, bs = 'sos', k = 500),
                      family = gaussian)
cv[1, 3] <- regional_cv(mgcv::gam, 
                        formula = log(EVENT50) ~ s(ELEVATION, k = 15) +
                          s(LATITUDE, LONGITUDE, bs = 'sos', k = 75),
                        family = gaussian)

cv[2, 1] <- "Kriging"
cv[2, 2] <- national_cv(krig)
cv[2, 3] <- regional_cv(krig)

cv[3, 1] <- "Prism"
cv[3, 2] <- national_cv(prism_function)
cv[3, 3] <- regional_cv(prism_function)

cv[4, 1] <- "OLS"
cv[4, 2] <- national_cv(lm, 
                        formula = log(EVENT50) ~ ELEVATION)
cv[4, 3] <- regional_cv(lm, 
                        formula = log(EVENT50) ~ ELEVATION)

save(cv, file = "snowload_methods.RData")

# Format and save file
cv[["Improvement"]] <- round(c(100 * (cv[, 2] - cv[, 3]) / cv[, 2]))
cv[, 2] <- round(100 * cv[, 2], 1)
cv[, 3] <- round(100 * cv[, 3], 1)
write_csv(cv, "snowload_methods.csv")


