library(tidyverse)
library(sf)
library(nngeo)

eco3_subset <- sf::read_sf("../data/NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp") %>%
  select(ECO3 = NA_L3CODE) %>%
  filter(ECO3 %in% c("10.1.4", "10.1.6", "10.1.7", "6.2.14", 
                     "9.4.1",  "9.4.1",  "9.4.3")) %>%
  sf::st_transform(eco3, crs = 4326) %>%
  mutate(AREA = sf::st_area(.)) %>%
  filter(as.numeric(AREA) > 2e9) %>%
  nngeo::st_remove_holes() 

set.seed(40)
contrived_points <- data.frame(
  LAT = runif(350, 31, 45),
  LON = runif(350, -114, -98)
) %>%
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) %>%
  filter(apply(sf::st_within(., eco3_subset, sparse = FALSE), 1, any))

mnt <- eco3_subset %>% filter(ECO3 == "6.2.14")

# warning given on st_buffer but we don't care since we aren't near a pole
mnt_buf <- mnt %>% sf::st_buffer(0.5) %>%
  sf::st_union() %>%
  sf::st_as_sf()

points_buf <- contrived_points %>%
  filter(apply(sf::st_within(contrived_points, mnt_buf, sparse = FALSE), 
               1, any))

png("buffer_example.png", width = 650, height = 600)
ggplot(eco3_subset) +
  geom_sf(fill = "NA") +
  geom_sf(data = mnt, color = "brown", fill = "NA") +
  geom_sf(data = mnt_buf, color = "brown", fill = "NA", 
          linetype = "dashed", size = .8) +
  geom_sf(data = contrived_points, size = 1.1) +
  geom_sf(data = points_buf, size = 1.1, color = "brown") +
  theme_void()
dev.off()





