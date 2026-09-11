# arcgistiles 0.1.0

* Initial release.
* `map_server()` and `vector_tile_server()` read a service and its tiling scheme.
* `tile_grid()`, `tile_extent()`, `lods()`, `level_for_size()`, and
  `level_for_resolution()` implement the Esri tiling scheme.
* `get_tiles()` and `get_vector_tiles()` download the tiles covering an extent.
* `as_rast()` converts downloaded tiles or an exported image into a
  georeferenced `SpatRaster`.
* `export_map()` renders an extent of a map service to an image file.
* `map_grid()` and `map_series()` export a series of map images at a fixed
  scale.
