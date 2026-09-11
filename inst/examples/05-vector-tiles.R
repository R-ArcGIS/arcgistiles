library(arcgistiles)

vts <- vector_tile_server(open_street_map_url())
vts

tile_info(vts)

style <- vector_tile_style(vts)
names(style)
style$sprite
style$glyphs
length(style$layers)

head(vector_tile_fonts(vts))

head(vector_tile_resources(vts), 4)

sprite <- vector_tile_sprite(vts)
file.size(sprite)

bbox <- sf::st_bbox(
  c(xmin = -71.2, ymin = 42.3, xmax = -71.0, ymax = 42.4),
  crs = sf::st_crs(4326)
)

tiles <- get_vector_tiles(vts, bbox, level = 12L, progress = FALSE)
tiles
file.size(tiles@tiles$path)
