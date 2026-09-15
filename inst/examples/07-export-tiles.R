library(arcgistiles)
library(arcgisutils)

set_arc_token(auth_user())

vts <- vector_tile_server(open_street_map_url())
vts@export_tiles_allowed
vts@max_export_tiles

job <- export_tiles_job(vts, vts@full_extent, levels = 0:2)
job

job$await(interval = 2, timeout = 600)
job$status

package <- write_tile_package(job)
file.size(package)
