library(arcgistiles)

styles <- basemap_styles()
nrow(styles)

table(styles$styleFamily)

basemap_styles("arcgis")[1:8, c("path", "name", "group")]

head(attr(styles, "languages"), 8)

attr(styles, "worldviews")

nrow(attr(styles, "languages"))
