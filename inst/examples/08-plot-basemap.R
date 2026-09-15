library(arcgistiles)
library(sf)
library(terra)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc <- st_transform(nc, 3857)

ms <- map_server(world_topo_map_url())

tiles <- get_tiles(ms, nc, size = c(1600, 800), progress = FALSE)
tiles

basemap <- as_rast(tiles)
basemap

out <- file.path(tempdir(), "nc-basemap.png")

png(out, width = 1600, height = 800)
plotRGB(basemap)
plot(st_geometry(nc), add = TRUE, border = "#d7191c", lwd = 2)
dev.off()

file.size(out)
