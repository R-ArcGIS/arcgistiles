# Examples

One runnable script per part of the package. Every script was run against live
services and its console output captured in `output/`.

| Script | What it covers |
|---|---|
| `01-map-service.R` | Opening cached and dynamic map services, reading the tiling scheme |
| `02-tiles.R` | Choosing a level, computing a tile grid, checking availability, downloading, merging to a raster |
| `03-export-map.R` | Exporting map images, layer filters, definition expressions |
| `04-map-series.R` | Building page grids and exporting a series at a fixed scale |
| `05-vector-tiles.R` | Vector tile services, styles, fonts, sprites, `.pbf` tiles |
| `06-basemap-styles.R` | Browsing the basemap styles catalogue |
| `07-export-tiles.R` | Asynchronous tile package export |
| `08-plot-basemap.R` | Plotting vector data over a downloaded basemap |
| `09-vector-tile-sf.R` | Decoding vector tiles into `sf` and drawing them over imagery |

Only `07-export-tiles.R` needs a token. The rest run against public services.

Run one with:

``` r
source(system.file("examples/02-tiles.R", package = "arcgistiles"), echo = TRUE)
```

## Georeferencing

`08-plot-basemap.R` is the end to end check. It downloads World Topo tiles for
the bounding box of the `sf` package's North Carolina counties and draws the
county boundaries on top.

The boundaries land on the coastline and the state border, which is only true
if the tiling scheme arithmetic, the CRS handling, and the raster extents all
agree.
