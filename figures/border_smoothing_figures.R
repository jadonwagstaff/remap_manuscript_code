library(tidyverse)
library(sf)
library(gridExtra)
library(remap)

# Data
# ==============================================================================
regions_example <- tibble::tribble(
  ~value, ~geometry,
  1, sf::st_polygon(list(matrix(c(0, 0, 2, 0, 6, 3, 4, 10, 0, 10, 0, 0)*.1, ncol = 2, byrow = TRUE))),
  2, sf::st_polygon(list(matrix(c(2, 0, 10, 0, 10, 4, 6, 3, 2, 0)*.1, ncol = 2, byrow = TRUE))),
  3, sf::st_polygon(list(matrix(c(4, 10, 6, 3, 10, 4, 10, 10, 4, 10)*.1, ncol = 2, byrow = TRUE)))
) %>%
  sf::st_as_sf(crs = 4326)

regions_gap <- tibble::tribble(
  ~value, ~geometry,
  1, sf::st_polygon(list(matrix(c(0, 0, 2, 0, 5.5, 2.625, 0, 10, 0, 0)*.1, ncol = 2, byrow = TRUE))),
  2, sf::st_polygon(list(matrix(c(2, 0, 10, 0, 10, 4, 6, 3, 2, 0)*.1, ncol = 2, byrow = TRUE))),
  3, sf::st_polygon(list(matrix(c(8, 10, 6.5, 3.125, 10, 4, 10, 10, 8, 10)*.1, ncol = 2, byrow = TRUE)))
) %>%
  sf::st_as_sf(crs = 4326)

regions_nondif <- tibble::tribble(
  ~value, ~geometry,
  1, sf::st_polygon(list(matrix(c(0, 0, 10, 0, 10, 10, 9, 10, 5, 1, 1, 10, 0, 10, 0, 0)*.1, ncol = 2, byrow = TRUE))),
  2, sf::st_polygon(list(matrix(c(1, 10, 5, 1, 9, 10, 1, 10)*.1, ncol = 2, byrow = TRUE)))
) %>%
  sf::st_as_sf(crs = 4326)

res <- 100
points <- data.frame(
  LONGITUDE = rep(0:res / res, times = res + 1),
  LATITUDE = rep(0:res / res, each = res + 1)
) %>%
  mutate(x = LONGITUDE, y = LATITUDE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)



# Make dummy models
# ==============================================================================
make_model <- function(x, const) {
  if (missing(const)) {
    class(x) <- "dummy_model"
  } else {
    class(x) <- "dummy_const"
  }
  return(x)
}
predict.dummy_model <- function(object, data) {
  x <- sf::st_coordinates(data)[, "X"]
  y <- sf::st_coordinates(data)[, "Y"]
  if (object[[1]] == 1) {
    x - y + 1
  } else if (object[[1]] == 2) {
    y - x + 1.4
  } else {
    x - y + 0.7
  }
}
predict.dummy_const <- function(object, data) {
  rep(object[[1]], nrow(data))
}

model_example <- list(
  models = list("1" = make_model(1),
                "2" = make_model(2),
                "3" = make_model(3)),
  regions = regions_example,
  region_id = "value"
)
class(model_example) <- "remap"

model_gap <- list(
  models = list("1" = make_model(0, "const"),
                "2" = make_model(2, "const"),
                "3" = make_model(4, "const")),
  regions = regions_gap,
  region_id = "value"
)
class(model_gap) <- "remap"

model_nondif <- list(
  models = list("1" = make_model(1, "const"),
                "2" = make_model(2, "const")),
  regions = regions_nondif,
  region_id = "value"
)
class(model_nondif) <- "remap"

points <- points %>%
  mutate(example = predict(model_example, points, 0.01),
         example_smooth = predict(model_example, points, 30),
         gap = predict(model_gap, points, 35),
         nondif = predict(model_nondif, points, 40))


# Plot
# ==============================================================================
png("example.png", width = 880, height = 750)
gridExtra::grid.arrange(
  ggplot2::ggplot(regions_example) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = example)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.7, 0.7)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    scale_fill_viridis_c(option = "inferno",
                         name = "Prediction") +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle("Regional models without smoothing"),
  
  ggplot(points %>% filter(LATITUDE == 0.7),
         aes(x = LONGITUDE, y = example)) +
    geom_line(color = "gray40", size = 1.1) +
    ylab("Prediction") +
    xlab("Longitude") +
    ggtitle("") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  
  ggplot2::ggplot(regions_example) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = example_smooth)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.7, 0.7)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    scale_fill_viridis_c(option = "inferno",
                         name = "Prediction", 
                         limits = c(0, 1.25)) +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    ggtitle("Regional models with smoothing"),
  
  ggplot(points %>% filter(LATITUDE == 0.7),
         aes(x = LONGITUDE, y = example_smooth)) +
    geom_line(color = "gray40", size = 1.1) +
    ylab("Prediction") +
    xlab("Longitude") +
    ggtitle("") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  ncol = 2,
  widths = c(0.6, 0.4)
)
dev.off()

png("gap.png", width = 880, height = 360)
gridExtra::grid.arrange(
  ggplot2::ggplot(regions_gap) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = gap)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.6, 0.6)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.95, 0.95)),
              aes(x = x, y = y), color = "gray40", size = 1.1,
              linetype = "dashed") +
    scale_fill_viridis_c(option = "inferno",
                         begin = .3, end = 1,
                         name = "Prediction") +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)),

  ggplot(points %>% filter(LATITUDE == 0.6),
         aes(x = LONGITUDE, y = gap)) +
    geom_line(color = "gray40", size = 1.1) +
    geom_line(data = points %>% filter(LATITUDE == 0.95),
              aes(x = LONGITUDE, y = gap),
              color = "gray40", size = 1.1,
              linetype = "dashed") +
    ylab("Prediction") +
    xlab("Longitude") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  ncol = 2,
  widths = c(0.6, 0.4)
)
dev.off()

png("nondiff.png", width = 880, height = 360)
gridExtra::grid.arrange(
  ggplot2::ggplot(regions_nondif) +
    geom_tile(data = points,
              aes(x = LONGITUDE, y = LATITUDE, fill = nondif)) +
    geom_sf(fill = NA, color = "black", size = 1) +
    geom_line(data = data.frame(x = c(0, 1), y = c(0.5, 0.5)),
              aes(x = x, y = y), color = "gray40", size = 1.1) +
    scale_fill_viridis_c(option = "inferno",
                         begin = .3, end = 1,
                         name = "Prediction") +
    ylab("Latitude") +
    xlab("Longitude") +
    theme_bw() +
    theme(text = element_text(size = 20)),

  ggplot(points %>% filter(LATITUDE == 0.5),
         aes(x = LONGITUDE, y = nondif)) +
    geom_line(color = "gray40", size = 1.1) +
    ylab("Prediction") +
    xlab("Longitude") +
    theme_minimal() +
    theme(text = element_text(size = 20)),
  ncol = 2,
  widths = c(0.6, 0.4)
)
dev.off()




# border example
line_points <- data.frame(
  LONGITUDE = -1000:1000 / 1000,
  LATITUDE = rep(0, 2001)
) %>%
  mutate(x = LONGITUDE, y = LATITUDE) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326)

line_regions <- tibble::tribble(
  ~value, ~geometry,
  1, sf::st_polygon(list(matrix(c(-1, -1, 0, -1, 0, 1, -1, 1, -1, -1), ncol = 2, byrow = TRUE))),
  2, sf::st_polygon(list(matrix(c(0, -1, 1, -1, 1, 1, 0, 1, 0, 1, 0, -1), ncol = 2, byrow = TRUE)))
) %>%
  sf::st_as_sf(crs = 4326)

line_model <- list(
  models = list("1" = make_model(1, "const"),
                "2" = make_model(2, "const")),
  regions = line_regions,
  region_id = "value"
)
class(line_model) <- "remap"

line_dist <- remap::redist(line_points, line_regions, value)

line_points <- line_points %>%
  mutate(Prediction = predict(line_model, line_points, 50),
         dist = -1 * as.numeric(line_dist[, 2]) + as.numeric(line_dist[, 1]))

png("border_example.png", width = 750, height = 300)
# warning for removed rows: limiting the size of the plot so it doesn't matter
ggplot(line_points, aes(x = dist, y = Prediction)) +
  geom_line(size = 1.1, color = "gray40") +
  geom_vline(xintercept = 0, size = 1) +
  scale_x_continuous(name = "Distance",
                     limits = c(-100, 100), 
                     labels = c("100", "50", "0", "50", "100")) +
  theme_minimal() +
  theme(text = element_text(size = 20))
dev.off()


