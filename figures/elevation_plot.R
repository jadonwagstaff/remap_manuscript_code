library(tidyverse)
library(sf)

# Get continental US shape file
loads <- read_csv("../data/us_50year_snow_loads.csv") %>%
  mutate(V = sample(1:10, nrow(.), replace = TRUE),
         x = LONGITUDE, y = LATITUDE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

png("elevation_plot.png", width = 750, height = 600)
loads %>%
  filter(STATE %in% c("WA", "MT")) %>%
  mutate(STATE = if_else(STATE == "WA", "Washington", "Montana"),
         STATE = factor(STATE, levels = c("Washington", "Montana"))) %>%
  ggplot(aes(x = ELEVATION, y = EVENT50)) +
  facet_wrap(vars(STATE), ncol = 1) +
  geom_smooth(method = "gam", se = FALSE, color = "brown") +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  xlab("Elevation (m)") +
  ylab("50 Year Load (kPa)") +
  theme(text = element_text(size = 20))
dev.off()
