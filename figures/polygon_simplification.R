library(tidyverse)

# Get continental US shape file
cont_us <- maps::map("state", plot = FALSE, fill = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_union()

# throws a warning about true assumption that attribute variables are assumed
# to be constant
# warns that st_simplify does not correctly simplify lon/lat data, this doesn't
# matter for this problem and gives us a close enough approximation
eco3 <- sf::read_sf("../../NA_CEC_Eco_Level3/NA_CEC_Eco_Level3.shp") %>%
  select(ECO3 = NA_L3CODE) %>%
  sf::st_transform(eco3, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_intersection(cont_us) %>%
  mutate(AREA = sf::st_area(.)) %>%
  filter(as.numeric(AREA) > 1e8) %>%
  select(-AREA)

eco3_simp <- eco3 %>%
  sf::st_simplify(dTolerance = 0.1)




full_small <- ggplot(eco3) +
  geom_sf() +
  coord_sf(xlim = c(-118, -112), ylim = c(43, 47)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "brown", fill=NA, size=2))

full_large <- ggplot(eco3) +
  geom_sf() +
  coord_sf(xlim = c(-150, -67), ylim = c(22, 50)) +
  geom_segment(x = -140.75, xend = -138.2, y = 27.1, yend = 27.1, size = 1) +
  geom_rect(xmin = -118, xmax = -112, ymin = 43, ymax = 47, 
            color = "brown", fill = NA, size = 1) +
  annotate("text", x=-139.5, y=26, label= "50 km", size = 6.5) + 
  ggtitle("Original polygons") +
  theme_void() +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))


simp_small <- ggplot(eco3_simp) +
  geom_sf() +
  coord_sf(xlim = c(-118, -112), ylim = c(43, 47)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "brown", fill=NA, size=2))

simp_large <- ggplot(eco3_simp) +
  geom_sf() +
  coord_sf(xlim = c(-150, -67), ylim = c(22, 50)) +
  geom_segment(x = -140.75, xend = -138.2, y = 27.1, yend = 27.1, size = 1) +
  geom_rect(xmin = -118, xmax = -112, ymin = 43, ymax = 47, 
            color = "brown", fill = NA, size = 1) +
  annotate("text", x=-139.5, y=26, label= "50 km", size = 6.5) + 
  ggtitle("Simplified polygons") +
  theme_void() +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

png(file = "polygon_simplification.png", width = 880, height = 770)
gridExtra::grid.arrange(
  cowplot::ggdraw() +
    cowplot::draw_plot(full_large) +
    cowplot::draw_plot(full_small, x = 0.02, y = 0.24, height = .62, width = .3),
  
  cowplot::ggdraw() +
    cowplot::draw_plot(simp_large) +
    cowplot::draw_plot(simp_small, x = 0.02, y = 0.24, height = .62, width = .3),
  
  ncol = 1
)
dev.off()

