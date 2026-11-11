library("shiny")
library("move2")
library("sf")
library("leaflet")
library("leaflet.extras")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

options(shiny.maxRequestSize = 100 * 1024^2) # allow polygon files up to 100 MB to be uploaded in the App

`%||%` <- function(x, y) if (is.null(x)) y else x

######### Helpers ##############

### helper 1: read a polygon boundary file (.zip shapefile or .gpkg)
read_polygon_upload <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  # zipped shapefile
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    td <- tempfile("polyzip_")
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(path, exdir = td)
    
    shp <- list.files(td, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp) == 0) return(NULL)
    
    poly <- tryCatch(sf::read_sf(shp[1]), error = function(e) NULL)
    return(poly)
  }
  
  # GeoPackage: read first polygon layer
  if (grepl("\\.gpkg$", path, ignore.case = TRUE)) {
    layers_info <- tryCatch(sf::st_layers(path), error = function(e) NULL)
    if (is.null(layers_info) || length(layers_info$name) == 0) return(NULL)
    
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

### helper 2: normalize and validate boundary geometry (EPSG:4326, polygons only)
normalize_boundary <- function(poly) {
  if (is.null(poly) || nrow(poly) == 0) return(NULL)
  
  poly <- sf::st_zm(poly)
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

### helper 3: convert one drawn feature on the map to a boundary
feature_to_boundary_sf <- function(feature, id = 1) {
  if (is.null(feature) || is.null(feature$geometry$type)) return(NULL)
  
  typ <- feature$geometry$type
  coords <- feature$geometry$coordinates
  geom <- NULL
  
  # polygon / rectangle
  if (typ %in% c("Polygon", "MultiPolygon")) {
    ring <- if (typ == "Polygon") coords[[1]] else coords[[1]][[1]]
    mat <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
    geom <- sf::st_sfc(sf::st_polygon(list(mat)), crs = 4326)
  }
  
  # circle
  if (typ == "Point" && !is.null(feature$properties$radius)) {
    lon <- coords[[1]]
    lat <- coords[[2]]
    radius_m <- as.numeric(feature$properties$radius)
    
    # choose projected CRS for circle buffer
    zone <- floor((lon + 180) / 6) + 1
    epsg <- if (lat >= 0) 32600 + zone else 32700 + zone
    
    pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    geom <- pt |>
      sf::st_transform(epsg) |>
      sf::st_buffer(dist = radius_m) |>
      sf::st_transform(4326)
  }
  
  if (is.null(geom)) return(NULL)
  
  sf::st_sf(shape_id = id, geometry = geom)
}

### helper 4: convert all drawn features to a boundary
draw_to_boundary_sf <- function(fc) {
  if (is.null(fc) || is.null(fc$features) || length(fc$features) == 0) return(NULL)
  
  parts <- lapply(seq_along(fc$features), function(i) {
    feature_to_boundary_sf(fc$features[[i]], id = i)
  })
  
  parts <- Filter(function(x) !is.null(x), parts)
  if (length(parts) == 0) return(NULL)
  
  normalize_boundary(do.call(rbind, parts))
}

### helper 5: flag locations (EPSG:4326) as inside (1) or outside (0) the boundary;
# within and shape_id are NA for empty locations or when there is no boundary
flag_points <- function(d_ll, bnd) {
  n <- nrow(d_ll)
  within <- rep(NA_integer_, n)
  shape_id <- rep(NA_character_, n)
  if (is.null(bnd) || nrow(bnd) == 0) return(list(within = within, shape_id = shape_id))
  
  keep <- which(!sf::st_is_empty(d_ll))
  if (length(keep) == 0) return(list(within = within, shape_id = shape_id))
  
  hits <- sf::st_within(sf::st_geometry(d_ll)[keep], sf::st_geometry(bnd))
  within[keep] <- as.integer(lengths(hits) > 0)
  shape_id[keep] <- vapply(hits, function(idx) paste(bnd$shape_id[idx], collapse = ","), character(1))
  
  list(within = within, shape_id = shape_id)
}

formatTime <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")

### helper 6: point popup
make_point_popup <- function(d) {
  xy <- sf::st_coordinates(d)
  shp <- ifelse(is.na(d$shape_id) | d$shape_id == "", "-", d$shape_id)
  
  paste0(
    "<b>Track:</b> ", d$track_id_chr, "<br>",
    "<b>Time (UTC):</b> ", formatTime(mt_time(d)), "<br>",
    "<b>Lon:</b> ", round(xy[, 1], 6), "<br>",
    "<b>Lat:</b> ", round(xy[, 2], 6), "<br>",
    "<b>Flag:</b> ", d$flag, "<br>",
    "<b>Polygon:</b> ", shp
  )
}

### helper 7: flagged points table for the CSV download
build_flagged_table <- function(d) {
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
  
  data.frame(
    track_id = d$track_id_chr,
    longitude = xy[, 1],
    latitude = xy[, 2],
    timestamp = formatTime(mt_time(d)),
    flag = d$flag,
    shape_id = ifelse(is.na(d$shape_id), "", d$shape_id),
    stringsAsFactors = FALSE
  )
}

### helper 8: number of locations inside/outside per track
build_summary <- function(d) {
  ids <- sort(unique(d$track_id_chr))
  do.call(rbind, lapply(ids, function(id) {
    f <- d$flag[d$track_id_chr == id]
    n_in <- sum(f == "inside")
    n_out <- sum(f == "outside")
    data.frame(
      "Track" = id,
      "Locations" = length(f),
      "Inside" = n_in,
      "Outside" = n_out,
      "Not flagged" = sum(f == "-"),
      "% inside" = if (n_in + n_out > 0) round(100 * n_in / (n_in + n_out), 1) else NA,
      check.names = FALSE
    )
  }))
}

### helper 9: base maps and layer control; osm = FALSE for the exported maps,
# because OpenStreetMap blocks tiles requested from a file opened from disk
add_base_maps <- function(map, overlayGroups = NULL, osm = TRUE) {
  if (osm) map <- addProviderTiles(map, "OpenStreetMap", group = "OpenStreetMap")
  map <- map %>%
    addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial")
  addLayersControl(map,
                   baseGroups = c(if (osm) "OpenStreetMap", "TopoMap", "Aerial"),
                   overlayGroups = overlayGroups,
                   options = layersControlOptions(collapsed = FALSE))
}

### helper 10: boundary, track lines and points; works for leaflet() maps and leafletProxy()
# the marker shape shows the flag (inside: solid, outside: hollow ring), the colour shows the track
add_geofence_layers <- function(map, d, tl, bnd) {
  if (!is.null(bnd) && nrow(bnd) > 0) {
    map <- addPolygons(map, data = bnd, fill = TRUE, weight = 3, color = "red", fillOpacity = 0.1, group = "Boundary")
  }
  if (!is.null(tl) && nrow(tl) > 0) {
    map <- addPolylines(map, data = tl, weight = 3, opacity = 0.95, color = ~line_color, group = "Track lines")
  }
  if (!is.null(d) && nrow(d) > 0) {
    map <- addCircleMarkers(map, data = d, radius = ~point_radius, stroke = TRUE, weight = ~border_weight,
                            opacity = 1, color = ~border_color, fillColor = ~point_color,
                            fillOpacity = ~fill_opacity, popup = ~popup_html, group = "Points")
  }
  map
}

### helper 11: bounding box of the shown locations, or NULL
data_bbox <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  bb <- sf::st_bbox(d)
  if (all(is.finite(bb))) bb else NULL
}

### helper 12: legend of the flag symbols
legend_symbol <- function(fill, border, border_px, size_px, opacity = 1, ring = "none") {
  side <- (14 - size_px) / 2
  sprintf(paste0("<span style='display:inline-block;box-sizing:border-box;width:%dpx;height:%dpx;border-radius:50%%;",
                 "background:%s;border:%spx solid %s;box-shadow:%s;opacity:%s;vertical-align:middle;",
                 "margin:0 %spx 2px %spx;'></span>"),
          size_px, size_px, fill, border_px, border, ring, opacity, side + 6, side + 1)
}

add_flag_legend <- function(map) {
  html <- paste0(
    "<div style='background:white;padding:6px 10px;border-radius:5px;box-shadow:0 1px 5px rgba(0,0,0,0.4);",
    "font:12px/20px Arial,sans-serif;'>",
    "<b>Flags</b> <span style='color:#666;'>(colour = track)</span><br>",
    legend_symbol("lightblue", "#000000", 3, 14), "inside<br>",
    legend_symbol("#FFFFFF", "lightblue", 3, 14), "outside<br>",
    legend_symbol("lightblue", "lightblue", 1, 10, opacity = 0.6), "not flagged",
    "</div>")
  addControl(map, html = html, position = "bottomright")
}

############################################### Interface #################################

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  
  tagList(
    titlePanel("Interactive Geofence"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4("Tracks"),
        uiOutput(ns("animals_ui")),
        fluidRow(
          column(6, actionButton(ns("select_all_animals"), "Select All", class = "btn-sm")),
          column(6, actionButton(ns("unselect_animals"), "Unselect All", class = "btn-sm"))
        ),
        
        hr(),
        
        h4("Polygon Boundary"),
        radioButtons(ns("boundary_method"), "Boundary source",
                     choices = c("Draw area on map" = "draw", "Upload area file" = "upload"), selected = "draw"),
        
        conditionalPanel(
          condition = "input.boundary_method == 'upload'", ns = ns,
          fileInput(
            ns("upload_user_polygon"),
            "Choose polygon file (.zip shapefile or .gpkg)",
            accept = c(".zip", ".gpkg")
          ),
          tags$div(
            style = "color:red; font-weight:600; margin-top:5px;",
            textOutput(ns("upload_msg"))
          ),
          helpText(textOutput(ns("upload_info"), inline = TRUE)),
          helpText("The file uploaded here is kept by 'Store settings'. Without an upload here, the boundary file from the App settings is used.")
        ),
        
        conditionalPanel(
          condition = "input.boundary_method == 'draw'", ns = ns,
          helpText("Draw mode supports polygon, rectangle, and circle.")
        ),
        
        # hidden input that keeps the drawn shapes (GeoJSON) for 'Store settings'
        tags$div(
          style = "display:none;",
          textInput(ns("drawn_geojson"), label = NULL, value = "")
        ),
        
        hr(),
        
        h4("Output"),
        radioButtons(ns("output_mode"), "Data passed on to the next App",
                     choices = c("All locations, with flag columns" = "annotate",
                                 "Only locations inside the boundary" = "keep_inside",
                                 "Only locations outside the boundary" = "keep_outside"),
                     selected = "annotate"),
        
        helpText("After changing tracks, boundary or output, click 'Flag Points' to apply."),
        actionButton(ns("apply_geofence"), "Flag Points", class = "btn-primary btn-block"),
        tags$div(
          style = "color:red; font-weight:600; margin-top:5px;",
          textOutput(ns("apply_msg"))
        ),
        
        hr(),
        
        h4("Download"),
        downloadButton(ns("save_html"), "Download map as HTML", class = "btn-sm"),
        br(), br(),
        downloadButton(ns("save_png"), "Download map as PNG", class = "btn-sm"),
        br(), br(),
        conditionalPanel(
          condition = "input.boundary_method == 'draw'", ns = ns,
          downloadButton(ns("download_draw_gpkg"), "Download drawn boundary (.gpkg)", class = "btn-sm"),
          br(), br()
        ),
        downloadButton(ns("download_flagged_data"), "Download selected flagged data CSV", class = "btn-sm")
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Map", shinycssloaders::withSpinner(leafletOutput(ns("leafmap"), height = "85vh"))),
          tabPanel("Summary", br(), tableOutput(ns("summary_table")))
        )
      )
    )
  )
}

############################################ Server ###################################

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  setBookmarkExclude(c("select_all_animals", "unselect_animals", "apply_geofence"))
  
  if (is.null(data) || nrow(data) == 0) {
    logger.warn("Input is NULL or has 0 rows. Returning NULL.")
    return(reactive(NULL))
  }
  
  if (!inherits(data, "move2")) stop("Input data must be a move2 object.")
  
  clash <- intersect(c("within", "shape_id"), names(data))
  if (length(clash) > 0) {
    logger.warn("The input data already contains the column(s) %s. They are overwritten.", paste(clash, collapse = ", "))
  }
  
  # longitude/latitude working copy for flagging and display;
  # the returned data keeps its original CRS
  if (is.na(sf::st_crs(data))) {
    logger.warn("The input data has no coordinate reference system. Longitude/latitude (EPSG:4326) is assumed.")
    data_ll <- sf::st_set_crs(data, 4326)
  } else {
    data_ll <- sf::st_transform(data, 4326)
  }
  
  track_col <- mt_track_id_column(data)
  track_ids <- as.character(mt_track_id(data))
  all_ids <- sort(unique(track_ids))
  nonempty <- !sf::st_is_empty(data_ll)
  
  # track colors
  track_cols <- stats::setNames(grDevices::hcl.colors(length(all_ids), palette = "Dark 3"), all_ids)
  
  ######## track selection ########
  output$animals_ui <- renderUI({
    # keep a restored selection if it is valid, otherwise select all tracks
    restored_sel <- isolate(input$animals)
    sel <- if (!is.null(restored_sel)) as.character(restored_sel) else all_ids
    sel <- intersect(sel, all_ids)
    
    checkboxGroupInput(
      ns("animals"),
      label = NULL,
      choiceNames = lapply(all_ids, function(id) {
        tags$span(tags$span(style = paste0("display:inline-block;", "width:12px;height:12px;", "background:", track_cols[[id]], ";",
                                           "border:1px solid black;", "margin-right:6px;")), id)
      }),
      choiceValues = all_ids,
      selected = sel
    )
  })
  
  observeEvent(input$select_all_animals, {
    updateCheckboxGroupInput(session, "animals", selected = all_ids)
  }, ignoreInit = TRUE)
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  }, ignoreInit = TRUE)
  
  live_animals <- reactive({
    intersect(as.character(input$animals), all_ids)
  })
  
  ######## drawn boundary ########
  # the drawn shapes are kept as GeoJSON in a hidden input, so that 'Store settings' restores them
  observeEvent(input$leafmap_draw_all_features, {
    fc <- input$leafmap_draw_all_features
    txt <- if (is.null(fc) || is.null(fc$features) || length(fc$features) == 0) {
      ""
    } else {
      as.character(jsonlite::toJSON(fc, auto_unbox = TRUE, digits = NA))
    }
    updateTextInput(session, "drawn_geojson", value = txt)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  drawn_boundary <- reactive({
    txt <- input$drawn_geojson
    if (is.null(txt) || !nzchar(txt)) return(NULL)
    
    fc <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(fc)) {
      logger.warn("The stored drawn boundary could not be read and is ignored.")
      return(NULL)
    }
    draw_to_boundary_sf(fc)
  })
  
  ######## uploaded boundary ########
  # boundary file from the App settings (USER_FILE 'boundary_file'); used when nothing is uploaded in the App
  settings_file <- getAuxiliaryFilePath("boundary_file")
  settings_boundary <- NULL
  if (!is.null(settings_file)) {
    settings_boundary <- normalize_boundary(read_polygon_upload(settings_file))
    if (is.null(settings_boundary)) {
      logger.warn("No polygons could be read from the boundary file of the App settings ('%s'). Please provide a .zip shapefile or a .gpkg with polygons.", basename(settings_file))
    } else {
      logger.info("Boundary file of the App settings read with %d polygon(s).", nrow(settings_boundary))
    }
  }
  
  # the file uploaded here is kept by 'Store settings' (the MoveApps SDK stores it with the settings)
  app_upload <- reactive({
    info <- input$upload_user_polygon
    if (is.null(info)) return(NULL)
    
    if (!tolower(tools::file_ext(info$name)) %in% c("zip", "gpkg")) {
      logger.info("Invalid upload '%s': only .zip or .gpkg allowed.", info$name)
      return(NULL)
    }
    
    poly <- normalize_boundary(read_polygon_upload(info$datapath))
    if (is.null(poly)) {
      logger.info("No polygons could be read from the uploaded file '%s'.", info$name)
    } else {
      logger.info("Uploaded boundary file '%s' read with %d polygon(s).", info$name, nrow(poly))
    }
    poly
  })
  
  output$upload_msg <- renderText({
    info <- input$upload_user_polygon
    if (is.null(info)) return("")
    if (!tolower(tools::file_ext(info$name)) %in% c("zip", "gpkg")) {
      "Please upload only a .zip shapefile or a .gpkg file."
    } else if (is.null(app_upload())) {
      "No polygons could be read from this file."
    } else {
      ""
    }
  })
  
  upload_boundary <- reactive({
    app_upload() %||% settings_boundary
  })
  
  output$upload_info <- renderText({
    if (!is.null(app_upload())) {
      paste0("Using the file uploaded here (", nrow(app_upload()), " polygon(s)).")
    } else if (!is.null(settings_boundary)) {
      paste0("Using the boundary file from the App settings (", nrow(settings_boundary), " polygon(s)).")
    } else {
      "No boundary file available."
    }
  })
  
  active_boundary <- reactive({
    switch(input$boundary_method %||% "draw",
           draw = drawn_boundary(),
           upload = upload_boundary(),
           NULL)
  })
  
  ######## flag points ########
  # the map, CSV, summary and returned data all use the last applied state
  applied <- reactiveVal(NULL)
  apply_msg <- reactiveVal("")
  
  do_apply <- function(animals) {
    bnd <- active_boundary()
    fl <- flag_points(data_ll, bnd)
    
    if (is.null(bnd)) {
      apply_msg("No boundary defined: locations are not flagged.")
      logger.info("No boundary defined: locations are not flagged.")
    } else {
      apply_msg("")
      logger.info("%d of %d locations are inside the boundary (%d polygon(s)).",
                  sum(fl$within == 1, na.rm = TRUE), nrow(data_ll), nrow(bnd))
    }
    
    applied(list(
      animals = animals,
      boundary = bnd,
      mode = input$output_mode %||% "annotate",
      within = fl$within,
      shape_id = fl$shape_id
    ))
  }
  
  # first result when the App opens, using restored settings if available
  observeEvent(TRUE, {
    do_apply(intersect(as.character(input$animals %||% all_ids), all_ids))
  }, once = TRUE)
  
  observeEvent(input$apply_geofence, {
    sel <- live_animals()
    if (length(sel) == 0) {
      apply_msg("Please select at least one track.")
      logger.info("No track selected. Nothing was applied.")
      return()
    }
    do_apply(sel)
  }, ignoreInit = TRUE)
  
  output$apply_msg <- renderText(apply_msg())
  
  ######## data for map, tables and downloads ########
  # selected tracks, non-empty locations, longitude/latitude, with flags and display columns
  display_data <- reactive({
    st <- applied()
    req(st)
    
    d <- data_ll
    d$within <- st$within
    d$shape_id <- st$shape_id
    d <- d[track_ids %in% st$animals & nonempty, ]
    if (nrow(d) == 0) return(d)
    
    d$track_id_chr <- as.character(mt_track_id(d))
    d <- d[order(d$track_id_chr, mt_time(d)), ]
    
    d$flag <- ifelse(is.na(d$within), "-", ifelse(d$within == 1, "inside", "outside"))
    
    # inside: solid dot with a white outline; outside: hollow ring; not flagged: small transparent dot
    trk_col <- unname(track_cols[d$track_id_chr])
    d$point_color <- ifelse(d$flag == "outside", "#FFFFFF", trk_col)
    d$border_color <- ifelse(d$flag == "inside", "#000000", trk_col)
    d$border_weight <- ifelse(d$flag == "outside", 3, 1)
    d$point_radius <- ifelse(d$flag == "-", 4, 6)
    d$fill_opacity <- ifelse(d$flag == "-", 0.6, 0.95)
    
    d$popup_html <- make_point_popup(d)
    d
  })
  
  # track lines of tracks with at least 2 locations
  track_lines_data <- reactive({
    d <- display_data()
    n_per_track <- table(d$track_id_chr)
    d <- d[d$track_id_chr %in% names(n_per_track)[n_per_track >= 2], ]
    if (nrow(d) < 2) return(NULL)
    
    tl <- mt_track_lines(d)
    tl$line_color <- unname(track_cols[as.character(tl[[track_col]])])
    tl
  })
  
  ######## map ########
  output$leafmap <- renderLeaflet({
    method <- input$boundary_method
    
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      add_base_maps(c("Boundary", "Track lines", "Points")) %>%
      addScaleBar(position = "topleft") %>%
      add_flag_legend()
    
    bb <- if (is.null(isolate(applied()))) NULL else data_bbox(isolate(display_data()))
    m <- if (is.null(bb)) {
      setView(m, lng = 0, lat = 20, zoom = 2)
    } else {
      fitBounds(m, as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
    }
    
    if (identical(method, "draw")) {
      m <- m %>%
        addDrawToolbar(
          targetGroup = "drawn_boundary",
          polygonOptions = drawPolygonOptions(showArea = TRUE),
          rectangleOptions = drawRectangleOptions(),
          circleOptions = drawCircleOptions(),
          polylineOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions()
        )
      
      # show restored or previously drawn shapes, so they can be edited or deleted
      bnd <- isolate(drawn_boundary())
      if (!is.null(bnd)) {
        m <- addPolygons(m, data = bnd, group = "drawn_boundary", color = "#3388ff", weight = 4, fillOpacity = 0.2)
      }
    }
    
    m
  })
  
  # update layers
  observe({
    d <- display_data()
    tl <- track_lines_data()
    bnd <- if (identical(input$boundary_method, "upload")) upload_boundary() else NULL
    
    leafletProxy("leafmap", session = session) %>%
      clearGroup("Boundary") %>%
      clearGroup("Track lines") %>%
      clearGroup("Points") %>%
      add_geofence_layers(d, tl, bnd)
  })
  
  # zoom to the shown locations only when the selection is applied
  observeEvent(applied(), {
    bb <- data_bbox(display_data())
    if (!is.null(bb)) {
      leafletProxy("leafmap", session = session) %>%
        fitBounds(as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
    }
  })
  
  ######## summary ########
  output$summary_table <- renderTable({
    d <- display_data()
    validate(need(nrow(d) > 0, "No locations to summarise. Select at least one track and click 'Flag Points'."))
    build_summary(d)
  })
  
  ######## export map for saving ########
  build_export_map <- function() {
    d <- display_data()
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      add_base_maps(c("Boundary", "Track lines", "Points"), osm = FALSE) %>%
      addScaleBar(position = "topleft") %>%
      add_geofence_layers(d, track_lines_data(), applied()$boundary) %>%
      add_flag_legend()
    
    bb <- data_bbox(d)
    if (!is.null(bb)) {
      m <- fitBounds(m, as.numeric(bb["xmin"]), as.numeric(bb["ymin"]), as.numeric(bb["xmax"]), as.numeric(bb["ymax"]))
    }
    m
  }
  
  ############################## downloads #############################
  
  #### map as HTML
  output$save_html <- downloadHandler(
    filename = function() paste0("Geofence_map_", Sys.Date(), ".html"),
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving HTML ...")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      htmlwidgets::saveWidget(build_export_map(), file = file, selfcontained = TRUE)
    }
  )
  
  #### map as PNG
  output$save_png <- downloadHandler(
    filename = function() paste0("Geofence_map_", Sys.Date(), ".png"),
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving PNG ...")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      # as in First Passage Time Segmentation: save the map in the working directory,
      # where the browser used by webshot2 can open it
      html_file <- paste0("Geofence_map_export_", session$token, ".html")
      on.exit(unlink(c(html_file, sub("\\.html$", "_files", html_file)), recursive = TRUE), add = TRUE)
      htmlwidgets::saveWidget(build_export_map(), file = html_file, selfcontained = TRUE)
      webshot2::webshot(url = html_file, file = file, vwidth = 1000, vheight = 800, delay = 2)
    }
  )
  
  #### drawn boundary as GeoPackage
  output$download_draw_gpkg <- downloadHandler(
    filename = function() paste0("drawn_boundary_", Sys.Date(), ".gpkg"),
    content = function(file) {
      bnd <- drawn_boundary()
      if (is.null(bnd)) {
        showNotification("There is no drawn boundary to download.", type = "warning")
        req(FALSE)
      }
      sf::st_write(bnd, file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    }
  )
  
  #### flagged locations of the selected tracks as CSV
  output$download_flagged_data <- downloadHandler(
    filename = function() paste0("flagged_points_", Sys.Date(), ".csv"),
    content = function(file) {
      out <- build_flagged_table(display_data())
      if (nrow(out) == 0) {
        utils::write.csv(data.frame(message = "No data available"), file, row.names = FALSE)
      } else {
        utils::write.csv(out, file, row.names = FALSE)
      }
    }
  )
  
  ######## returned data ########
  # all tracks, original CRS, with the columns within and shape_id;
  # filtered to inside/outside locations if selected
  result <- reactive({
    st <- applied()
    if (is.null(st)) return(data)
    
    d <- data
    d$within <- st$within
    d$shape_id <- st$shape_id
    
    if (st$mode == "annotate") return(d)
    
    if (is.null(st$boundary)) {
      logger.warn("No boundary defined. The data are passed on without filtering.")
      return(d)
    }
    
    keep <- if (st$mode == "keep_inside") which(d$within == 1) else which(d$within == 0)
    if (length(keep) == 0) {
      logger.warn("No locations are left after filtering. Returning NULL.")
      return(NULL)
    }
    d[keep, ]
  })
  
  return(result)
}