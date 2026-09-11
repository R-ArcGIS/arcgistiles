library(arcgistiles)
library(sf)
library(terra)

vts <- vector_tile_server(open_street_map_url())

bbox <- wk::rct(-71.12, 42.34, -71.05, 42.38, crs = 4326)

tiles <- get_vector_tiles(vts, bbox, level = 14L, progress = FALSE)
tiles

layers <- vector_tile_layers(tiles)
length(layers)
head(layers, 10)

parts <- read_vector_tiles(tiles, layers = c("road", "water area"), crs = 3857)
lengths(parts)

roads <- parts[["road"]]
roads

ms <- map_server(world_imagery_url())
imagery <- as_rast(get_tiles(ms, bbox, level = 14L, progress = FALSE))

out <- "boston-vector-tiles.png"

png(out, width = 1200, height = 900)
plotRGB(imagery)
plot(st_geometry(parts[["water area"]]), add = TRUE, col = "#2c7fb888", border = NA)
plot(st_geometry(roads), add = TRUE, col = "#ffd92f", lwd = 1.5)
dev.off()

file.size(out)
