library(shiny)
library(move2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(shinycssloaders)

options(shiny.maxRequestSize = 100 * 1024^2)

######### Helpers

###### helper1: Read uploaded polygon boundary
read_polygon_upload <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  # zip shapefile
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

########helper 2: Normalize and validate boundary geometry
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


############### helper3: Convert drawn feature on map to boundary
feature_to_boundary_sf <- function(feature, id = 1) {
  if (is.null(feature) || is.null(feature$geometry$type)) return(NULL)
  
  typ <- feature$geometry$type
  coords <- feature$geometry$coordinates
  geom <- NULL
  
  # Polygon -Rectangle
  if (typ %in% c("Polygon", "MultiPolygon")) {
    ring <- if (typ == "Polygon") coords[[1]] else coords[[1]][[1]]
    mat <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
    geom <- sf::st_sfc(sf::st_polygon(list(mat)), crs = 4326)
  }
  
  # Circle
  if (typ == "Point" && !is.null(feature$properties$radius)) {
    lon <- coords[[1]]
    lat <- coords[[2]]
    radius_m <- as.numeric(feature$properties$radius)
    
    #Select projected CRS for circle buffer
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

#### helper 4: convert all drawn features to boundary
feature_collection_to_boundary_sf <- function(fc) {
  if (is.null(fc) || is.null(fc$features) || length(fc$features) == 0) return(NULL)
  
  parts <- lapply(seq_along(fc$features), function(i) {
    feature_to_boundary_sf(fc$features[[i]], id = i)
  })
  
  parts <- Filter(function(x) !is.null(x), parts)
  if (length(parts) == 0) return(NULL)
  
  normalize_boundary(do.call(rbind, parts))
}
######## helper 5:Extract coordinates of all drawn shapes#############

extract_all_draw_coords <- function(fc) {
  if (is.null(fc) || is.null(fc$features) || length(fc$features) == 0) return(NULL)
  
  out <- list()
  for (i in seq_along(fc$features)) {
    feature <- fc$features[[i]]
    typ <- feature$geometry$type
    coords <- feature$geometry$coordinates
    
    if (typ %in% c("Polygon", "MultiPolygon")) {
      ring <- if (typ == "Polygon") coords[[1]] else coords[[1]][[1]]
      mat <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
      
      df <- data.frame(shape_id = i, shape_type = typ, vertex = seq_len(nrow(mat)), longitude = round(mat[, 1], 6), latitude = round(mat[, 2], 6), radius_m = NA_real_)
      
      if (nrow(df) > 1 && df$longitude[1] == df$longitude[nrow(df)] &&  df$latitude[1] == df$latitude[nrow(df)]) {
        df <- df[-nrow(df), ]
      }
      
      out[[length(out) + 1]] <- df
    }
    
    if (typ == "Point" && !is.null(feature$properties$radius)) {
      out[[length(out) + 1]] <- data.frame(shape_id = i,shape_type = "Circle",vertex = 1, longitude = round(coords[[1]], 6), latitude = round(coords[[2]], 6),  radius_m = as.numeric(feature$properties$radius))
    }
  }
  
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}


######### helper 6:points popup
make_point_popup <- function(d) {
  xy <- sf::st_coordinates(d)
  tt <- as.character(mt_time(d))
  paste0( "<b>Time:</b> ", tt, "<br>","<b>Lon:</b> ", round(xy[, 1], 6), "<br>","<b>Lat:</b> ", round(xy[, 2], 6), "<br>","<b>Flag:</b> ", d$flag  )
}

###### helper 7:Build flagged points table
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
  
  hits <- sf::st_intersects(d, boundary_sf)
  inside <- lengths(hits) > 0
  
  out$flag <- ifelse(inside, "inside", "outside")
  out$shape_id[inside] <- vapply(hits[inside], function(idx) {
    paste(boundary_sf$shape_id[idx], collapse = ",")
  }, character(1))
  
  out
}

############################################### Interface #################################


shinyModuleUserInterface <- function(id, label = NULL, ...) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Geofence"),
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
        radioButtons(ns("boundary_method"),"Boundary source", choices = c("Draw area on map" = "draw", "Upload area file" = "upload" ), selected = "draw" ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("boundary_method")),
          fileInput( ns("upload_user_polygon"), "Choose polygon file (.zip shapefile or .gpkg)",  accept = c(".zip", ".gpkg") )
        ),
        
        helpText("Draw mode supports polygon, rectangle, and circle."),
        
        br(),
        actionButton(ns("apply_geofence"), "Flag Points",class = "btn-primary btn-block"),
        
        br(), br(),
        h5("Drawn shape coordinates"),
        tableOutput(ns("draw_coords_tbl")),
        
        hr(),
        downloadButton(ns("save_html"), "Download HTML", class = "btn-sm"),
        downloadButton(ns("save_png"), "Download PNG", class = "btn-sm"),
        downloadButton(ns("download_draw_coords"), "Download drawn coordinates table", class = "btn-sm"),
        downloadButton(ns("download_flagged_data"), "Download flagged data", class = "btn-sm")
      ),
      
      mainPanel(withSpinner(leafletOutput(ns("leafmap"), height = "85vh")), width = 9)
    )
  )
}


############################################ Server ###################################


shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  
  if (is.null(data) || nrow(data) == 0) {
    message("Input is NULL or has 0 rows.")
    return(reactive(NULL))
  }
  
  if (!inherits(data, "sf")) {
    stop("Input data must be an sf/move2 object.")
  }
  
  if (!sf::st_is_longlat(data)) {
    data <- sf::st_transform(data, 4326)
  }
  
  track_col <- mt_track_id_column(data)
  all_ids <- sort(unique(as.character(data[[track_col]])))
  
  
  # Track selection
  
  output$animals_ui <- renderUI({
    checkboxGroupInput( ns("animals"),  label = NULL, choices = all_ids, selected = all_ids  )
  })
  
  observeEvent(input$select_all_animals, {
    updateCheckboxGroupInput(session, "animals", selected = all_ids)
  }, ignoreInit = TRUE)
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  }, ignoreInit = TRUE)
  
  
  # Selected original data
  
  selected_data <- reactive({
    sel <- input$animals
    if (is.null(sel) || length(sel) == 0) return(data[0, ])
    
    d <- data[data[[track_col]] %in% sel, ]
    d <- d[!sf::st_is_empty(d), ]
    d
  })
  

  # Track colors
  
  track_cols <- reactive({
    d <- selected_data()
    ids <- sort(unique(as.character(d[[track_col]])))
    cols <- grDevices::hcl.colors(length(ids), palette = "Dark 3")
    names(cols) <- ids
    cols
  })
  
  
  # Drawn boundary + coords
  
  drawn_boundary <- reactiveVal(NULL)
  drawn_coords <- reactiveVal(NULL)
  applied <- reactiveVal(FALSE)
  
  output$draw_coords_tbl <- renderTable({
    drawn_coords()
  }, rownames = FALSE)
  
  observeEvent(input$leafmap_draw_all_features, {
    fc <- input$leafmap_draw_all_features
    
    if (is.null(fc) || is.null(fc$features) || length(fc$features) == 0) {
      drawn_boundary(NULL)
      drawn_coords(NULL)
    } else {
      drawn_boundary(feature_collection_to_boundary_sf(fc))
      drawn_coords(extract_all_draw_coords(fc))
    }
    
    applied(FALSE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$boundary_method, {
    applied(FALSE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$upload_user_polygon, {
    applied(FALSE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$apply_geofence, {
    applied(TRUE)
  }, ignoreInit = TRUE)
  
  
  # Uploaded boundary
  
  upload_boundary <- reactive({
    info <- input$upload_user_polygon
    if (is.null(info)) return(NULL)
    
    poly <- read_polygon_upload(info$datapath)
    normalize_boundary(poly)
  })
  
  
  # Active boundary
  
  active_boundary <- reactive({
    if (is.null(input$boundary_method)) return(NULL)
    
    if (input$boundary_method == "upload") {
      return(upload_boundary())
    }
    
    if (input$boundary_method == "draw") {
      return(drawn_boundary())
    }
    
    NULL
  })
  
  
  # Flagged table for CSV download
  # always computed from current boundary
  
  flagged_table <- reactive({
    d <- selected_data()
    bnd <- active_boundary()
    build_flagged_table(d, bnd, track_col)
  })
  
  
  # Data for map display only
  # green/red only after Apply
  
  display_data <- reactive({
    d <- selected_data()
    if (nrow(d) == 0) return(d)
    
    d$track_id_chr <- as.character(d[[track_col]])
    d$track_color <- unname(track_cols()[d$track_id_chr])
    
    if (isTRUE(applied())) {
      tab <- build_flagged_table(d, active_boundary(), track_col)
      d$flag <- tab$flag
      d$shape_id <- tab$shape_id
    } else {
      d$flag <- "-"
      d$shape_id <- ""
    }
    
    # keep original track color as point fill
    d$point_color <- d$track_color
    
    # border color only for flagging
    d$border_color <- ifelse(
      d$flag == "inside", "black",
      ifelse(d$flag == "outside", "gold", d$track_color)
    )
    
    d$border_weight <- ifelse(d$flag %in% c("inside", "outside"), 3, 1)
    
    d$popup_html <- make_point_popup(d)
    d
  })
  
  
  # Track lines
  
  track_lines_data <- reactive({
    d <- selected_data()
    if (nrow(d) < 2) return(NULL)
    
    ord <- order(as.character(d[[track_col]]), mt_time(d))
    d <- d[ord, ]
    
    tl <- tryCatch(mt_track_lines(d), error = function(e) NULL)
    if (is.null(tl) || nrow(tl) == 0) return(NULL)
    
    tl$line_id <- as.character(tl[[track_col]])
    tl$line_color <- unname(track_cols()[tl$line_id])
    
    tl
  })
  
  
  # Base map
  
  output$leafmap <- renderLeaflet({
    method <- input$boundary_method
    
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addScaleBar(position = "topleft")
    
    if (!is.null(method) && method == "draw") {
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
    }
    
    m %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        overlayGroups = c("Track lines", "Points"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("black", "gold"),
        labels = c("inside border", "outside border"),
        title = "Geofence"
      )
  })
  
  
  # Update layers
  
  observe({
    d <- display_data()
    tl <- track_lines_data()
    bnd <- active_boundary()
    method <- input$boundary_method
    
    proxy <- leafletProxy("leafmap", session = session)
    
    proxy %>%
      clearGroup("Boundary") %>%
      clearGroup("Track lines") %>%
      clearGroup("Points")
    
    # uploaded boundary is shown as static layer
    # drawn boundary is managed by draw toolbar
    if (!is.null(method) && method == "upload" && !is.null(bnd) && nrow(bnd) > 0) {
      proxy %>%
        addPolygons(
          data = bnd,
          fill = FALSE,
          weight = 3,
          color = "dodgerblue",
          group = "Boundary"
        )
    }
    
    if (!is.null(tl) && nrow(tl) > 0) {
      proxy %>%
        addPolylines(
          data = tl,
          weight = 3,
          opacity = 0.95,
          color = ~line_color,
          group = "Track lines"
        )
    }
    
    if (!is.null(d) && nrow(d) > 0) {
      proxy %>%
        addCircleMarkers(
          data = d,
          radius = 6,
          stroke = TRUE,
          weight = ~border_weight,
          color = ~border_color,
          fillColor = ~point_color,
          fillOpacity = 0.95,
          popup = ~popup_html,
          group = "Points"
        )
      
      bb <- tryCatch(sf::st_bbox(d), error = function(e) NULL)
      if (!is.null(bb) && all(is.finite(bb))) {
        proxy %>%
          fitBounds(
            lng1 = as.numeric(bb["xmin"]),
            lat1 = as.numeric(bb["ymin"]),
            lng2 = as.numeric(bb["xmax"]),
            lat2 = as.numeric(bb["ymax"])
          )
      }
    }
  })
  
  
  # Export map
  
  build_export_map <- function() {
    d <- display_data()
    tl <- track_lines_data()
    bnd <- active_boundary()
    
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addScaleBar(position = "topleft")
    
    if (!is.null(bnd) && nrow(bnd) > 0) {
      m <- m %>%
        addPolygons(
          data = bnd,
          fill = FALSE,
          weight = 3,
          color = "dodgerblue"
        )
    }
    
    if (!is.null(tl) && nrow(tl) > 0) {
      m <- m %>%
        addPolylines(
          data = tl,
          weight = 3,
          opacity = 0.95,
          color = ~line_color
        )
    }
    
    if (!is.null(d) && nrow(d) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = d,
          radius = 6,
          stroke = TRUE,
          weight = ~border_weight,
          color = ~border_color,
          fillColor = ~point_color,
          fillOpacity = 0.95,
          popup = ~popup_html
        )
      
      bb <- tryCatch(sf::st_bbox(d), error = function(e) NULL)
      if (!is.null(bb) && all(is.finite(bb))) {
        m <- m %>%
          fitBounds(
            lng1 = as.numeric(bb["xmin"]),
            lat1 = as.numeric(bb["ymin"]),
            lng2 = as.numeric(bb["xmax"]),
            lat2 = as.numeric(bb["ymax"])
          )
      }
    }
    
    m
  }
  
  output$save_html <- downloadHandler(
    filename = function() paste0("Geofence_map_", Sys.Date(), ".html"),
    content = function(file) {
      htmlwidgets::saveWidget(build_export_map(), file = file, selfcontained = TRUE)
    }
  )
  
  output$save_png <- downloadHandler(
    filename = function() paste0("Geofence_map_", Sys.Date(), ".png"),
    content = function(file) {
      html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(build_export_map(), file = html_file, selfcontained = TRUE)
      webshot2::webshot(html_file, file = file, vwidth = 1400, vheight = 900)
    }
  )
  
  
  # Download drawn coordinate table
  
  output$download_draw_coords <- downloadHandler(
    filename = function() {
      paste0("drawn_coordinates_", Sys.Date(), ".csv")
    },
    content = function(file) {
      coords <- drawn_coords()
      
      if (is.null(coords) || nrow(coords) == 0) {
        write.csv(data.frame(message = "No drawn shape available"), file, row.names = FALSE)
      } else {
        write.csv(coords, file, row.names = FALSE)
      }
    }
  )
  
  
  # Download flagged data CSV
  # track_id, longitude, latitude, timestamp, flag, shape_id
  
  output$download_flagged_data <- downloadHandler(
    filename = function() {
      paste0("flagged_points_", Sys.Date(), ".csv")
    },
    content = function(file) {
      out <- flagged_table()
      
      if (is.null(out) || nrow(out) == 0) {
        write.csv(data.frame(message = "No data available"), file, row.names = FALSE)
      } else {
        write.csv(out, file, row.names = FALSE)
      }
    }
  )
  
  
  # Return original selected data unchanged
  
  current <- reactive({
    selected_data()
  })
  
  return(reactive({
    req(current())
    current()
  }))
}