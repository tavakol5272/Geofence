library(shiny)
library(move2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(webshot2)
library(shinycssloaders)
library(shinybusy)
library(grDevices)

options(shiny.maxRequestSize = 100 * 1024^2) ###increase the size of uploaded file

`%||%` <- function(x, y) if (is.null(x)) y else x

######### Helpers

# helper 1: read uploaded polygon boundary
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

# helper 2: normalize and validate boundary geometry
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

# helper 3: convert one drawn feature on map to boundary
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

# helper 4: convert all drawn features to boundary
draw_to_boundary_sf <- function(fc) {
  if (is.null(fc) || is.null(fc$features) || length(fc$features) == 0) return(NULL)
  
  parts <- lapply(seq_along(fc$features), function(i) {
    feature_to_boundary_sf(fc$features[[i]], id = i)
  })
  
  parts <- Filter(function(x) !is.null(x), parts)
  if (length(parts) == 0) return(NULL)
  
  normalize_boundary(do.call(rbind, parts))
}

# helper 5: point popup
make_point_popup <- function(d) {
  xy <- sf::st_coordinates(d)
  tt <- as.character(mt_time(d))
  
  paste0(
    "<b>Time:</b> ", tt, "<br>",
    "<b>Lon:</b> ", round(xy[, 1], 6), "<br>",
    "<b>Lat:</b> ", round(xy[, 2], 6), "<br>",
    "<b>Flag:</b> ", d$flag
  )
}


# helper 6: build flagged points table
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
  
  # projected intersection for more stable inside/outside checks
  d_proj <- sf::st_transform(d, 3857)
  bnd_proj <- sf::st_transform(boundary_sf, 3857)
  
  hits <- sf::st_intersects(d_proj, bnd_proj)
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
        
        # hidden json-friendly track selection
        tags$div(
          style = "display:none;",
          textInput(ns("animals_json"), label = NULL, value = "")
        ),
        ########
        hr(),
        
        h4("Polygon Boundary"),
        radioButtons(ns("boundary_method"),"Boundary source", choices = c("Draw area on map" = "draw", "Upload area file" = "upload"), selected = "draw" ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("boundary_method")),
          fileInput(
            ns("upload_user_polygon"),
            "Choose polygon file (.zip shapefile or .gpkg)",
            accept = c(".zip", ".gpkg")
          ),
          tags$div(
            style = "color:#b30000; font-weight:600; margin-top:5px;",
            textOutput(ns("upload_msg"))
          )
        ),
        
        helpText("Draw mode supports polygon, rectangle, and circle."),
        helpText("After changing track selection, click 'Flag Points' to update the flagged map."),
        
        br(),
        actionButton(ns("apply_geofence"), "Flag Points", class = "btn-primary btn-block"),
        
        hr(),
        
        fluidRow(
          column(6, downloadButton(ns("save_html"), "Download HTML", class = "btn-sm")),
          column(6, downloadButton(ns("save_png"), "Download PNG", class = "btn-sm"))
        ),
        
        br(),
        
        fluidRow(
          conditionalPanel( condition = sprintf("input['%s'] == 'draw'", ns("boundary_method")),
            column(6, downloadButton(  ns("download_draw_gpkg"),  "Download Drawn boundary (.gpkg)", class = "btn-sm" ) )  ),
          column(6, downloadButton(ns("download_flagged_data"), "Flagged data", class = "btn-sm"))
        )
      ),
      
      mainPanel(
        withSpinner(leafletOutput(ns("leafmap"), height = "85vh")),
        width = 9
      )
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
  
  if (!inherits(data, "sf")) { stop("Input data must be an sf/move2 object.")  }
  
  if (!sf::st_is_longlat(data)) { data <- sf::st_transform(data, 4326)  }
  
  track_col <- mt_track_id_column(data)
  all_ids <- sort(unique(as.character(data[[track_col]])))
  
  
  ############
  # track colors
  track_cols <- reactive({
    cols <- grDevices::hcl.colors(length(all_ids), palette = "Dark 3")
    names(cols) <- all_ids
    cols
  })
  #################
  output$animals_ui <- renderUI({
    restored_sel <- isolate(input$animals)
    sel <- if (!is.null(restored_sel)) as.character(restored_sel) else all_ids
    sel <- intersect(sel, all_ids)
    
    cols <- track_cols()
    
    checkboxGroupInput(
      ns("animals"),
      label = NULL,
      choiceNames = lapply(all_ids, function(id) {
        this_col <- cols[id]
        if (length(this_col) == 0 || is.na(this_col)) this_col <- "gray"
        
        tags$span(tags$span(style = paste0("display:inline-block;", "width:12px;height:12px;", "background:", this_col, ";", "border:1px solid black;", "margin-right:6px;" )  ), id)
      }),
      choiceValues = all_ids,
      selected = sel
    )
  })
  
  ########################
  observeEvent(input$select_all_animals, {
    updateCheckboxGroupInput(session, "animals", selected = all_ids)
  }, ignoreInit = TRUE)
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  }, ignoreInit = TRUE)
  
  # save selected animals as json-friendly hidden input
  observeEvent(input$animals, {
    vals <- as.character(input$animals %||% character(0))
    updateTextInput(session, "animals_json", value = jsonlite::toJSON(vals, auto_unbox = FALSE))
  }, ignoreInit = TRUE)
  
  # give me the current animal selection
  live_animals <- reactive({
    sel <- as.character(input$animals)
    if (is.null(sel)) sel <- all_ids
    intersect(sel, all_ids)
  })
  
  # after clicking Flag Points, the map uses this frozen set
  applied_animals <- reactiveVal(all_ids)
  applied <- reactiveVal(FALSE)
  
  init_applied <- reactiveVal(FALSE)
  ###################
  observeEvent(input$animals, {
  req(!is.null(input$animals))
  applied_animals(as.character(input$animals))
  init_applied(TRUE)
}, ignoreInit = FALSE, once = TRUE)
  #####################
  # drawn / uploaded boundary
  drawn_boundary <- reactiveVal(NULL)
  
  observeEvent(input$leafmap_draw_all_features, {
    fc <- input$leafmap_draw_all_features
    
    if (is.null(fc) || is.null(fc$features) || length(fc$features) == 0) {
      drawn_boundary(NULL)
    } else {
      drawn_boundary(draw_to_boundary_sf(fc))
    }
    
    applied(FALSE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$boundary_method, {  applied(FALSE) }, ignoreInit = TRUE)
  
  observeEvent(input$upload_user_polygon, { applied(FALSE)}, ignoreInit = TRUE)
  
  ###############
  upload_msg <- reactiveVal("")
  
  observeEvent(input$upload_user_polygon, {
    info <- input$upload_user_polygon
    if (is.null(info)) return()
    
    if (!tolower(tools::file_ext(info$name)) %in% c("zip", "gpkg")) {
      logger.info("Invalid upload '%s': only .zip or .gpkg allowed.", info$name)
      upload_msg("Please upload only a .zip shapefile or a .gpkg file.")
    } else {
      upload_msg("")
    }
  })
  
  output$upload_msg <- renderText(upload_msg())
  ############################################
  
  observeEvent(input$apply_geofence, {
    applied_animals(live_animals())
    applied(TRUE)
  }, ignoreInit = TRUE)
  
  upload_boundary <- reactive({
    info <- input$upload_user_polygon
    if (is.null(info)) return(NULL)
    
    poly <- read_polygon_upload(info$datapath)
    normalize_boundary(poly)
  })
  
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
  
  # current animals 
  current_animals <- reactive({
    applied_animals()
  })
  
  
  # take the full dataset, keep only the currently selected animals
  selected_data <- reactive({
    req(init_applied())
    
    sel <- current_animals()
    if (is.null(sel) || length(sel) == 0) return(data[0, ])
    
    d <- data[as.character(data[[track_col]]) %in% sel, ]
    d <- d[!sf::st_is_empty(d), ]
    d
  })
  
  
  
  
  
  ###################
  # flagged table for CSV download
  
  flagged_table <- reactive({
    d <- selected_data()
    bnd <- active_boundary()
    build_flagged_table(d, bnd, track_col)
  })
  
  
  # data for map display only
  
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
    
    d$point_color <- d$track_color
    d$border_color <- ifelse(
      d$flag == "inside", "black",
      ifelse(d$flag == "outside", "gold", d$track_color)
    )
    d$border_weight <- ifelse(d$flag %in% c("inside", "outside"), 4, 3)
    d$popup_html <- make_point_popup(d)
    
    d
  })
  
 
  # track lines
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
  
 
  # base map
  
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
      addLegend(  position = "bottomright", colors = c("black", "gold"),labels = c("inside border", "outside border"),  title = "Flags")
  })
 
  # update layers
  
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
    
    if (!is.null(method) && method == "upload" && !is.null(bnd) && nrow(bnd) > 0) {
      proxy %>%
        addPolygons(  data = bnd, fill = TRUE, weight = 3, color = "red",  group = "Boundary"  )
    }
    
    if (!is.null(tl) && nrow(tl) > 0) {
      proxy %>%
        addPolylines( data = tl,weight = 3, opacity = 0.95, color = ~line_color,group = "Track lines"   )
    }
    
    if (!is.null(d) && nrow(d) > 0) {
      proxy %>%
        addCircleMarkers( data = d, radius = 6, stroke = TRUE, weight = ~border_weight, color = ~border_color, fillColor = ~point_color, fillOpacity = 0.95, popup = ~popup_html, group = "Points")
      
      bb <- tryCatch(sf::st_bbox(d), error = function(e) NULL)
      if (!is.null(bb) && all(is.finite(bb))) {
        proxy %>%
          fitBounds(lng1 = as.numeric(bb["xmin"]),lat1 = as.numeric(bb["ymin"]),lng2 = as.numeric(bb["xmax"]),lat2 = as.numeric(bb["ymax"])  )
      }
    }
  })
  
 
  # export map for saving 
  
  build_export_map <- function() {
    d <- display_data()
    tl <- track_lines_data()
    bnd <- active_boundary()
    
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addScaleBar(position = "topleft")
    if (!is.null(bnd) && nrow(bnd) > 0) {
      m <- m %>%
        addPolygons(data = bnd, fill = TRUE,  weight = 3,color = "red"   )
    }
    if (!is.null(tl) && nrow(tl) > 0) {
      m <- m %>%
        addPolylines(  data = tl, weight = 3,opacity = 0.95, color = ~line_color  )
    }
    if (!is.null(d) && nrow(d) > 0) {
      m <- m %>%
        addCircleMarkers(data = d,radius = 6,stroke = TRUE, weight = ~border_weight, color = ~border_color, fillColor = ~point_color, fillOpacity = 0.95, popup = ~popup_html )
      
      bb <- tryCatch(sf::st_bbox(d), error = function(e) NULL)
      if (!is.null(bb) && all(is.finite(bb))) {
        m <- m %>%
          fitBounds( lng1 = as.numeric(bb["xmin"]), lat1 = as.numeric(bb["ymin"]), lng2 = as.numeric(bb["xmax"]), lat2 = as.numeric(bb["ymax"]))
      }
    }
    
    m
  }
  
  
  ############################## downloads #############################
  
  
  #### download HTML
  output$save_html <- downloadHandler(
    filename = function() paste0("Geofence_map_", Sys.Date(), ".html"),
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving HTML…")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      saveWidget(build_export_map(), file = file, selfcontained = TRUE)
    }
  )
  
  ### save map as PNG
  output$save_png <- downloadHandler(
    filename = function() paste0("Geofence_map_", Sys.Date(), ".png"),
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving PNG…")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      html_file <- "leaflet_export.html"
      htmlwidgets::saveWidget(build_export_map(), file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot2::webshot( url = html_file,file = file,vwidth = 1000, vheight = 800 )
    }
  )
  
  ### save polygons as gpkg
  output$download_draw_gpkg <- downloadHandler(
    filename = function() {
      paste0("drawn_boundary_", Sys.Date(), ".gpkg")
    },
    content = function(file) {
      bnd <- drawn_boundary()
      if (is.null(bnd) || nrow(bnd) == 0) return()
      
      sf::st_write(bnd, file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    }
  )
  
  # save data with flags
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
  
  
  # return data
  
  return(reactive({
    req(data)
    data
  }))
}