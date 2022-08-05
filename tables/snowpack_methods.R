library(tidyverse)
library(sf)
library(raster)
library(maps)
library(mgcv)
library(automap)
library(remap)

# Code was originally written before sf version 1.0.0. The new version uses 
# S2 to calculate spherical geometries which introduces a whole host of bugs
# into the code. Turning S2 off fixes these bugs.
sf::sf_use_s2(FALSE)


# Load data
# ==============================================================================

# Make cross validation column for apr1 data and convert to sf object
set.seed(42)
apr1 <- read_csv("../data/ut_apr1_snow_pack.csv") %>%
  filter(YEAR <= 2015, YEAR >= 1986) %>%
  mutate(WESD = WESD + 1, 
         V = sample(1:10, nrow(.), replace = TRUE),
         x = LONGITUDE, y = LATITUDE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

# Get utah shape file
ut <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326) %>%
  filter(ID == "utah")

# Get watersheds for remap
# Warns that st_simplify does not correctly buffer lon/lat data, this doesn't
# matter for this problem and gives us a close enough approximation.
# Throws a warning about true assumption that attribute variables are assumed
# to be constant.
ws <- rbind(
  sf::st_read("../data/watersheds/WBD_14_HU2_Shape/WBDHU4.shp"),
  sf::st_read("../data/watersheds/WBD_15_HU2_Shape/WBDHU4.shp"),
  sf::st_read("../data/watersheds/WBD_16_HU2_Shape/WBDHU4.shp"),
  sf::st_read("../data/watersheds/WBD_17_HU2_Shape/WBDHU4.shp")
) %>%
  sf::st_transform(4326) %>%
  sf::st_intersection(sf::st_buffer(ut, 1.2)) %>%
  mutate(HUC4 = as.character(HUC4),
         HUC2 = substring(HUC4, 1, 2)) %>%
  dplyr::select(HUC4, HUC2)

# Simplify watersheds
# Simplification and distances needs to be shown with the new S2 calculations
sf::sf_use_s2(TRUE)
ws_simp <- ws %>%
  sf::st_simplify(dTolerance = 5000) %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  sf::st_cast("MULTIPOLYGON")


# General model building and evaluation functions
# ==============================================================================

# Cross validation function for HUC remap model
# Parameter - modeling function
# Returns - cross validated MSE
regional_sp_cv <- function(fun, huc, dist, ...) {
  
  preds <- rep(as.numeric(NA), nrow(apr1))
  
  years <- sort(unique(apr1$YEAR))
  
  for (i in 1:length(years)) {
    # Make a subset index for each year
    year_index <- apr1$YEAR == years[i]
    
    # do cross validation for each year
    for (k in 1:max(as.numeric(apr1$V))) {
      pred_index <- apr1$V == k
      
      models <- remap::remap(apr1[year_index & !pred_index, ],
                             regions = ws_simp, region_id = huc,
                             model_function = fun,
                             buffer = 20, min_n = 30,
                             distances = dist[year_index & !pred_index, ],
                             ...)
      
      preds[year_index & pred_index] <- predict(
        models, 
        apr1[year_index & pred_index, ],
        smooth = 10, 
        distances = dist[year_index & pred_index, ]
      )
    }
  }
  
  c(MSE = mean((preds - log(apr1$WESD))^2))
}

# Cross validation function for state level model
# Parameter - modeling function
# Returns - cross validated MSE
state_cv <- function(fun, ...) {
  preds <- rep(as.numeric(NA), nrow(apr1))
  
  years <- sort(unique(apr1$YEAR))
  
  for (i in 1:length(years)) {
    # Make a subset index for each year
    year_index <- apr1$YEAR == years[i]
    
    # do cross validation for each year
    for (k in 1:max(as.numeric(apr1$V))) {
      pred_index <- apr1$V == k
      
      model <- fun(apr1[year_index & !pred_index, ], ...)
      
      preds[year_index & pred_index] <- predict(
        model, 
        apr1[year_index & pred_index, ]
      )
    }
  }
  
  c(MSE = mean((preds - log(apr1$WESD))^2))
}

# A bounded prediction function that uses the highest value in the modeling
# data as a bound
predict.bound <- function(object, data) {
  preds <- predict(object$model, data)
  preds[preds < log(1)] <- log(1)
  preds[preds > log(object$ubound)] <- log(object$ubound)
  return(preds)
}


# OLS model functions for Table 3
# ==============================================================================
ols_ut <- function(data, ...) {
  ubound <- max(data$WESD)
  
  model <- lm(data, ...)
  
  out <- list(model = model, ubound = ubound)
  class(out) <- "bound"
  
  return(out)
}


# GAM model functions for Table 3
# ==============================================================================
gam_ut <- function(data, ...) {
  ubound <- max(data$WESD)
  
  model <- mgcv::gam(data, ...)
  
  out <- list(model = model, ubound = ubound)
  class(out) <- "bound"
  
  return(out)
}

# Kriging model functions for Table 3
# ==============================================================================
krig_ut <- function(data) {
  ubound <- max(data$WESD)
  
  fml <- log(WESD) ~ ELEVATION
  data <- data %>%
    sf::st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-100 +lat_0=45") %>%
    sf::as_Spatial()
  
  # This is to avoid warnings from sp::proj4string which are letting the 
  # user know that proj4string is outdated. This doesn't affect the output
  raster::crs(data) <- NA
  
  model <- list(data = data, fml = fml)
  class(model) <- "krig"
  
  out <- list(model = model, ubound = ubound)
  class(out) <- "bound"
  
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


# Make distance matrix and evaluation data frame
# ==============================================================================
dists2 <- remap::redist(apr1, ws_simp, HUC2)
dists4 <- remap::redist(apr1, ws_simp, HUC4)


# Do cross validation for different modeling approaches
# ==============================================================================
cv <- data.frame(
  Model = rep("", 3),
  State = rep(as.numeric(NA), 3),
  HUC2 = rep(as.numeric(NA), 3),
  HUC4 = rep(as.numeric(NA), 3),
  stringsAsFactors = FALSE
)

cv[1, 1] <- "GAM"
cv[1, 2] <- state_cv(gam_ut, 
                     formula = log(WESD) ~ s(ELEVATION, k = 15) + 
                       s(LATITUDE, LONGITUDE, bs = 'sos', k = 45),
                     family = gaussian)
cv[1, 3] <- regional_sp_cv(gam_ut, "HUC2", dists2,
                          formula = log(WESD) ~ s(ELEVATION, k = 5) +
                            s(LATITUDE, LONGITUDE, bs = 'sos', k = 20),
                          family = gaussian)
cv[1, 4] <- regional_sp_cv(gam_ut, "HUC4", dists4,
                          formula = log(WESD) ~ s(ELEVATION, k = 5) +
                            s(LATITUDE, LONGITUDE, bs = 'sos', k = 20),
                          family = gaussian)

cv[2, 1] <- "Kriging"
cv[2, 2] <- state_cv(krig_ut)
cv[2, 3] <- regional_sp_cv(krig_ut, "HUC2", dists2)
cv[2, 4] <- regional_sp_cv(krig_ut, "HUC4", dists4)

cv[3, 1] <- "OLS"
cv[3, 2] <- state_cv(ols_ut, 
                     formula = log(WESD) ~ ELEVATION)
cv[3, 3] <- regional_sp_cv(ols_ut, "HUC2", dists2,
                           formula = log(WESD) ~ ELEVATION)
cv[3, 4] <- regional_sp_cv(ols_ut, "HUC4", dists4,
                           formula = log(WESD) ~ ELEVATION)

save(cv, file = "snowpack_methods.RData")

# Format and save file
cv[, 2] <- round(cv[, 2] * 100)
cv[, 3] <- round(cv[, 3] * 100)
cv[, 4] <- round(cv[, 4] * 100)
write_csv(cv, "snowpack_methods.csv")







