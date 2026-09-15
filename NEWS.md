# arcgistiles 0.1.0

* Initial release.

## Services

* `map_server()` and `vector_tile_server()` open a map service or vector tile
  service and read its tiling scheme.
* `tile_info()`, `lods()`, `level_for_size()`, and `level_for_resolution()`
  describe a cache. `tile_grid()` and `tile_extent()` compute which tiles cover
  an extent, and `tilemap()` asks a service which of them it actually holds.

## Tiles and images

* `get_tiles()` and `get_vector_tiles()` download the tiles covering an extent
  in parallel. Missing tiles are reported in the `ok` column rather than
  raising an error.
* `export_map()` renders an extent of a dynamic service to an image, and
  `map_grid()` plus `map_series()` export a series of pages at one scale.
* `as_rast()` turns downloaded tiles or an exported image into a georeferenced
  `terra` `SpatRaster`.

## Vector tiles and basemaps

* `read_vector_tiles()` decodes downloaded tiles into `sf`, one data frame per
  layer, and `vector_tile_layers()` lists what a tile set holds. Needs the
  protolite package.
* `vector_tile_style()`, `vector_tile_resources()`, `vector_tile_fonts()`,
  `vector_tile_sprite()`, and `vector_tile_font()` read a vector tile service's
  style and resources.
* `basemap_styles()` browses the basemap styles service without a token.
  `basemap_style()`, `basemap_webmap()`, `basemap_tile_server()`, and
  `basemap_session()` need one.

## Tile export

* `export_tiles()` packages a service's cache for offline use. Use
  `export_tiles_job()` and `write_tile_package()` to drive the job yourself,
  and `estimate_export_tiles_size()` to check the download size first.
* `export_tiles_job()` returns an `arcgisutils::arc_gp_job`, so a tile export is
  polled with `job$status`, `job$await()` and `job$results` like every other
  asynchronous ArcGIS job.

## Known issues

* `basemap_tile_server()` fails against `basemaps-api.arcgis.com`, the vector
  tile host behind the ArcGIS basemap styles. That host rejects a bearer token
  sent as a header and accepts it only as a query parameter, so it is
  unreachable through the `arcgisutils` request path. Vector tile services on
  other hosts, such as `basemaps.arcgis.com`, work normally.
