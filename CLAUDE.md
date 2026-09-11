# CLAUDE.md

## About

`arcgistiles` reads ArcGIS map services, vector tile services, and the basemap
styles service. It downloads raster and vector tiles, exports map images
singly or as a series, and assembles them into georeferenced rasters.

It sits on top of `arcgisutils` for authentication and request construction and
uses `S7` plus `s7x` for its classes.

## Coding Standards

Follow the conventions in `../arcgisutils/CLAUDE.md` and `../s7x/CLAUDE.md`.
The ones that bite most often here:

- **Authentication belongs to `arcgisutils`.** Every request goes through
  `arc_base_req()`, which sets the `X-Esri-Authorization` bearer header. Do not
  add a second auth path, and do not pass tokens in the query string.
- **Never use `jsonlite`.** Read with `RcppSimdJson::fparse()`, write with
  `yyjsonr::write_json_str(x, auto_unbox = TRUE)`.
- **Docs are short.** One line per `@param`, shaped `@param name Type. Definition.`
- `@inheritParams` rather than repeating a parameter description.
- rlang standalone checks for every argument, `cli::cli_abort(call = error_call)`
  for every error.
- No ` - ` (spaced en/em dash) anywhere.

## Architecture

### Tiling scheme

`TileInfo` holds an Esri tiling scheme: `origin` (the upper left corner of the
grid), `rows`/`cols` (tile size in pixels), and `lods` (level, resolution,
scale). All tile arithmetic derives from those:

```
tile_width  = resolution[level] * cols
col         = floor((x - origin_x) / tile_width)
row         = floor((origin_y - y) / tile_height)
```

Rows increase downward from the origin. `span_indices()` applies a
`sqrt(.Machine$double.eps)` tolerance, without which a bounding box taken from
a tile's own extent returns that tile plus its neighbour.

`inst/examples/08-plot-basemap.R` is the regression test for all of this: it
draws county boundaries over downloaded tiles, and they only line up if the
tiling scheme, CRS handling, and raster extents all agree.

### Coordinate reference systems

`sf::st_crs()` objects expose `epsg` only through `$`, never `[[`. `crs$epsg`
works, `crs[["epsg"]]` silently returns `NULL`. `input` and `wkt` are real list
elements and work with either.

A `bbox` whose CRS has no EPSG code cannot be named in a query, so
`bbox_query()` transforms it into the service's CRS instead.

### Service quirks

- `export` returns an extent expanded from the requested one to match the
  aspect ratio of `size`. Always request `f=json` first and georeference from
  the returned `extent`, never from the requested bbox.
- Vector tile services return the export download URL on the job resource as
  `output.outputUrl`. Map services expose it at
  `exportTiles/jobs/<id>/results/out_service_url`. `job_result()` handles both.
- A style document's `defaultStyles` property gives the right path. The
  documented `resources/style` is wrong for some services, `resources/styles`
  is correct.
- `basemaps-api.arcgis.com` (the vector tile host behind basemap styles)
  returns `499 Token Required` for the `X-Esri-Authorization` header and only
  accepts a `token` query parameter. This is not worked around in the package.
  See the note in `NEWS.md`.
- A `MIXED` cache serves PNG or JPEG per tile, so the extension is only known
  once the bytes are on disk. `resolve_mixed()` sniffs the magic bytes.
- Esri returns errors as HTTP 200 with an `{"error": {...}}` body, so
  `arcgisutils::detect_errors()` runs on every parsed response.

### Classes

| Class | Purpose |
|---|---|
| `TileInfo` | A tiling scheme |
| `Service` | Abstract parent of the two service classes |
| `MapServer` | A map service |
| `VectorTileServer` | A vector tile service |
| `TileSet` | Downloaded tiles on disk |
| `MapImage` | An exported map image and the extent it covers |
| `TileExportJob` | An asynchronous tile package export |

A property typed as a nullable S3 class must put `NULL` first in the union,
`S7::new_union(NULL, S7::new_S3_class("crs"))`. The other order makes S7
generate a constructor default that cannot be deparsed, which `R CMD check`
reports as a codoc mismatch.

### Testing

Tile arithmetic is tested offline against a synthetic Web Mercator
`TileInfo()` built by `web_mercator_info()` in `tests/testthat/helper-tiles.R`.
Network tests call `skip_if_no_network()` and assert only that the essential
fields are present, never the exact shape of a response, so they do not break
when a service changes.
