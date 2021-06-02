library(tidyverse)
library(sf)
library(maps)

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
# matter for this problem and gives us a close enough approximation
# Throws a warning about true assumption that attribute variables are assumed
# to be constant
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
  select(HUC4, HUC2)

# Warns that st_simplify does not correctly simplify lon/lat data, this doesn't
# matter for this problem and gives us a close enough approximation
ws_simp <- ws %>%
  sf::st_simplify(dTolerance = .05) 



# Plot
# ==============================================================================

# Get watersheds for plot
# Warns that st_simplify does not correctly buffer lon/lat data, this doesn't
# matter for this problem and gives us a close enough approximation
# Throws a warning about true assumption that attribute variables are assumed
# to be constant
ut_only_ws  <- ws %>% sf::st_intersection(ut)

png("utah_regions.png", width = 500, height = 500)
ggplot(ut) +
  geom_sf(data = ut_only_ws, aes(fill = HUC2), size = 1) +
  geom_sf(fill = NA, size = 1, color = "black") +
  scale_fill_manual(values = c("gray95", "gray80", "gray65", "gray50")) +
  theme_void() +
  theme(text = element_text(size = 20),
        legend.position = "none")
dev.off()




