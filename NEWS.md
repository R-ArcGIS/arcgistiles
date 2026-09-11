# arcgistiles 0.1.0

* Initial release.

## Services

* `map_server()` and `vector_tile_server()` read a service and its tiling
  scheme into `MapServer` and `VectorTileServer` objects.
* `service_bbox()` returns a service's full extent as a `bbox`.

## Tiling scheme

* `tile_grid()`, `tile_extent()`, `lods()`, `level_for_size()`, and
  `level_for_resolution()` implement the Esri tiling scheme. All of it is
  arithmetic on `TileInfo`, so a tile grid can be inspected before anything is
  downloaded.
* `tilemap()` asks a service which tiles its cache actually holds.

## Tiles

* `get_tiles()` and `get_vector_tiles()` download the tiles covering an extent
  in parallel. Tiles missing from a cache are reported in the `ok` column
  rather than raising an error.
* `as_rast()` merges downloaded tiles, or an exported image, into a
  georeferenced `SpatRaster`.

## Map images and series

* `export_map()` renders an extent of a map service to an image file and
  records the extent the server actually drew.
* `map_grid()` and `map_series()` export a series of images at a fixed scale,
  the equivalent of a map series or data driven pages. Pages can come from a
  grid, an `sf` object, or a data frame of extents.

## Vector tiles and basemaps

* `vector_tile_style()`, `vector_tile_resources()`, `vector_tile_fonts()`,
  `vector_tile_sprite()`, and `vector_tile_font()` read a vector tile service's
  style and resources.
* `basemap_styles()`, `basemap_languages()`, and `basemap_worldviews()` browse
  the basemap styles service without a token. `basemap_style()`,
  `basemap_webmap()`, and `basemap_session()` need one.

## Tile export

* `export_tiles_job()`, `job_await()`, and `write_tile_package()` run an
  asynchronous tile package export. `export_tiles()` does all three at once and
  `estimate_export_tiles_size()` reports the download size first.

## Known issues

* `basemap_tile_server()` fails against `basemaps-api.arcgis.com`, the vector
  tile host behind the ArcGIS basemap styles. That host returns
  `499 Token Required` for a bearer token sent as `X-Esri-Authorization` or
  `Authorization`, and accepts the same token only as a `token` query
  parameter. Since authentication is `arcgisutils`' job and it sends the
  header, this is left as a service side inconsistency to raise with Esri
  rather than worked around here. Vector tile services on other hosts, such as
  `basemaps.arcgis.com`, honour the header normally.
