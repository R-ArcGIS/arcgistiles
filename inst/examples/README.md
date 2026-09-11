# Examples

Runnable scripts covering each part of the package. Every script was run
against live services and its console output captured in `output/`.

| Script | What it covers | Output |
|---|---|---|
| `01-map-service.R` | Opening cached and dynamic map services, reading the tiling scheme | `output/01-map-service.txt` |
| `02-tiles.R` | Choosing a level, computing a tile grid, checking availability, downloading, merging to a raster | `output/02-tiles.txt` |
| `03-export-map.R` | Exporting map images, layer filters, definition expressions | `output/03-export-map.txt` |
| `04-map-series.R` | Building page grids and exporting a series at a fixed scale | `output/04-map-series.txt` |
| `05-vector-tiles.R` | Vector tile services, styles, fonts, sprites, `.pbf` tiles | `output/05-vector-tiles.txt` |
| `06-basemap-styles.R` | Browsing the basemap styles catalogue | `output/06-basemap-styles.txt` |
| `07-export-tiles.R` | Asynchronous tile package export | `output/07-export-tiles.txt` |
| `08-plot-basemap.R` | Plotting vector data over a downloaded basemap | `output/nc-basemap.png` |

Only `07-export-tiles.R` needs a token. The rest run against public services.

Run one with:

``` r
source(system.file("examples/02-tiles.R", package = "arcgistiles"), echo = TRUE)
```

## Georeferencing

`08-plot-basemap.R` is the end to end check. It downloads World Topo tiles for
the bounding box of the `sf` package's North Carolina counties and draws the
county boundaries on top. The boundaries land on the coastline and state
border, which is only true if the tiling scheme arithmetic, the CRS handling,
and the raster extents all agree.

![North Carolina counties over an ArcGIS topographic basemap](output/nc-basemap.png)
