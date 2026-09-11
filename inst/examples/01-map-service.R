library(arcgistiles)

ms <- map_server(world_imagery_url())
ms

tile_info(ms)

head(lods(ms), 6)

service_bbox(ms)

ms@capabilities

ms@layers[1:5, c("id", "name")]

census <- map_server(census_url())
census

census@cached
