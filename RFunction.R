library(move2)
library(sf)
library(ggplot2)


######### Helpers ##############

### helper 1: read uploaded polygon boundary
read_polygon_upload <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  # zipped shapefile
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    td <- tempfile("polyzip_")
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    unzip(path, exdir = td)
    
    shp <- list.files(td, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp) == 0) return(NULL)
    
    poly <- tryCatch(sf::read_sf(shp[1]), error = function(e) NULL)
    return(poly)
  }
  
  # GeoPackage: read first polygon layer
  if (grepl("\\.gpkg$", path, ignore.case = TRUE)) {
    layers_info <- tryCatch(sf::st_layers(path), error = function(e) NULL)
    if (is.null(layers_info) || nrow(layers_info) == 0) return(NULL)
    
    for (lyr in layers_info$name) {
      obj <- tryCatch(sf::read_sf(path, layer = lyr), error = function(e) NULL)
      if (is.null(obj) || nrow(obj) == 0) next
      
      gtypes <- unique(as.character(sf::st_geometry_type(obj)))
      if (all(gtypes %in% c("POLYGON", "MULTIPOLYGON"))) {
        return(obj)
      }
    }
    
    return(NULL)
  }
  
  NULL
}

### helper 2: normalize and validate boundary geometry
normalize_boundary <- function(poly) {
  if (is.null(poly) || nrow(poly) == 0) return(NULL)
  
  poly <- tryCatch(sf::st_make_valid(poly), error = function(e) poly)
  
  if (is.na(sf::st_crs(poly))) {
    sf::st_crs(poly) <- 4326
  }
  
  poly <- sf::st_transform(poly, 4326)
  
  gtypes <- unique(as.character(sf::st_geometry_type(poly)))
  if (!all(gtypes %in% c("POLYGON", "MULTIPOLYGON"))) return(NULL)
  
  poly$shape_id <- seq_len(nrow(poly))
  poly
}


### helper 3: build flagged points table
build_flagged_table <- function(d, boundary_sf, track_col) {
  if (is.null(d) || nrow(d) == 0) {
    return(data.frame(
      track_id = character(0),
      longitude = numeric(0),
      latitude = numeric(0),
      timestamp = character(0),
      flag = character(0),
      shape_id = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  xy <- sf::st_coordinates(d)
  
  out <- data.frame(
    track_id = as.character(d[[track_col]]),
    longitude = xy[, 1],
    latitude = xy[, 2],
    timestamp = as.character(mt_time(d)),
    flag = "-",
    shape_id = "",
    stringsAsFactors = FALSE
  )
  
  if (is.null(boundary_sf) || nrow(boundary_sf) == 0) {
    return(out)
  }
  
  
  hits <- sf::st_within(d, boundary_sf)
  #hits <- sf::st_intersects(d, boundary_sf)
  #hits <- sf::st_covered_by(d, boundary_sf)
  inside <- lengths(hits) > 0
  
  out$flag <- ifelse(inside, "inside", "outside")
  out$shape_id[inside] <- vapply(hits[inside], function(idx) {
    paste(boundary_sf$shape_id[idx], collapse = ",")
  }, character(1))
  
  out
}


rFunction <- function(data, polygon, ...) {
  
  if (is.null(data) || nrow(data) == 0) {
    logger.info("Input is NULL or has 0 rows. Returning input data.")
    return(data)
  }
  
  if (!inherits(data, "sf")) {
    stop("Input data must be an sf/move2 object.")
  }
  
  if (!sf::st_is_longlat(data)) {
    data <- sf::st_transform(data, 4326)
  }
  
  track_col <- mt_track_id_column(data)
  
  polygon_path <- getAuxiliaryFilePath("polygon")
  
  if (is.null(polygon_path) || !file.exists(polygon_path)) {
    logger.info("No polygon file found. Returning input data.")
    return(data)
  }
  
  if (!tolower(tools::file_ext(polygon_path)) %in% c("zip", "gpkg")) {
    logger.info("Invalid polygon file. Only .zip and .gpkg are supported.")
    return(data)
  }
  
  boundary_sf <- read_polygon_upload(polygon_path)
  
  if (is.null(boundary_sf)) {
    logger.info("No valid polygon boundary found. Returning input data.")
    return(data)
  }
  
  boundary_sf <- normalize_boundary(boundary_sf)
  
  if (is.null(boundary_sf)) {
    logger.info("Uploaded boundary is not a valid polygon. Returning input data.")
    return(data)
  }
  
  # full data with one extra column: within
  full_flagged_data <- data
  full_flagged_data$within <- NA_integer_
  
  keep <- !sf::st_is_empty(full_flagged_data)
  
  if (any(keep)) {
    hits <- sf::st_within(full_flagged_data[keep, ], boundary_sf)
    inside <- lengths(hits) > 0
    full_flagged_data$within[keep] <- as.integer(inside)
  }
  
  data_sf <- full_flagged_data
  trck_data <- tryCatch(mt_track_lines(full_flagged_data), error = function(e) NULL)
  
  # flagged table artifact
  flagged_table <- build_flagged_table(full_flagged_data, boundary_sf, track_col)
  
  if (!is.null(flagged_table) && nrow(flagged_table) > 0) {
    write.csv(flagged_table, appArtifactPath("flagged_points.csv"), row.names = FALSE)
  }
  
  
  ##create a label column
  label_sf <- tryCatch(sf::st_point_on_surface(boundary_sf), error = function(e) NULL)
  if (!is.null(label_sf) && nrow(label_sf) > 0) {
    label_sf$poly_label <- paste("Polygon", label_sf$shape_id)
  }
  
  qc_plot <- ggplot() +
    geom_sf(data = boundary_sf, fill = "grey85", color = "black", linewidth = 0.4) +
    geom_sf(data = data_sf, aes(color = factor(within)), size = 0.8, alpha = 0.8 ) +
    scale_color_manual(
      values = c("0" = "red", "1" = "darkgreen"),
      labels = c("0" = "Outside", "1" = "Inside"),
      name = "Point status"
    ) +
    labs( x = "Longitude",y = "Latitude",
      title = "Geofence classification check"
    ) +
    theme_bw()
  
  
  if (!is.null(trck_data) && nrow(trck_data) > 0) {
    qc_plot <- qc_plot +
      geom_sf(data = trck_data, color = "steelblue", linewidth = 0.4, alpha = 0.7)
  }
  
  if (!is.null(label_sf) && nrow(label_sf) > 0) {
    qc_plot <- qc_plot +
      geom_sf_text(data = label_sf, aes(label = poly_label), size = 3)
  }
  
  ggsave(appArtifactPath("geofence_check.png"), plot = qc_plot,  width = 9, height = 6, units = "in", dpi = 300)
  return(full_flagged_data)
}