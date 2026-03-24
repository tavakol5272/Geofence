library(sf)
library(zip)

poly_big <- st_polygon(list(rbind(
  c(20, 55),
  c(40, 55),
  c(40, 68),
  c(20, 68),
  c(20, 55)
)))

big_sf <- st_sf(
  id = 1,
  geometry = st_sfc(poly_big, crs = 4326)
)


if (file.exists("big_test_polygon.gpkg")) file.remove("big_test_polygon.gpkg")
st_write(big_sf, "big_test_polygon.gpkg", quiet = TRUE)


dir.create("big_test_shp", showWarnings = FALSE)
old_files <- list.files("big_test_shp", full.names = TRUE)
if (length(old_files)) file.remove(old_files)

st_write(big_sf, "big_test_shp/big_test_polygon.shp", quiet = TRUE)

s
if (file.exists("big_test_polygon.zip")) file.remove("big_test_polygon.zip")
zip::zip(
  zipfile = "big_test_polygon.zip",
  files = list.files("big_test_shp", full.names = TRUE)
)