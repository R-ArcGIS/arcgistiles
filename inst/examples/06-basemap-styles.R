library(arcgistiles)

styles <- basemap_styles()
nrow(styles)

table(styles$styleFamily)

basemap_styles("arcgis")[1:8, c("path", "name", "group")]

head(basemap_languages(), 8)

basemap_worldviews()

basemap_url()
