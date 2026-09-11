library(arcgistiles)

ms <- map_server(world_imagery_url())

tile_info(ms)

lods(ms)
head(lods(ms), 6)

ms@full_extent

ms@capabilities

ms@metadata$layers[1:5, c("id", "name")]

census <- map_server(census_url())
census

census@cached
