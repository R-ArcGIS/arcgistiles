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

bbox <- wk::rct(-71.2, 42.3, -71.0, 42.4, crs = 4326)

tiles <- get_vector_tiles(vts, bbox, level = 12L, progress = FALSE)
tiles
file.size(tiles@tiles$path)
