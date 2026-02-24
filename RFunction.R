###R shiny
library(shiny)
library(move2)
library(lubridate)
library(sf)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(shinybusy)
library(leaflet.extras)
library(pals)
library(shinycssloaders)

####helper 1: read the uploaded shape
read_polygon_upload <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  # ZIP shapefile
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    td <- tempfile("polyzip_")
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
    unzip(path, exdir = td)
    
    shp <- list.files(td, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp) == 0) return(NULL)
    
    poly <- sf::read_sf(shp[1])
    return(poly)
  }
  
  # GeoPackage
  if (grepl("\\.gpkg$", path, ignore.case = TRUE)) {
    layers <- tryCatch(sf::st_layers(path)$name, error = function(e) character(0))
    if (length(layers) == 0) return(NULL)
    
    # read first layer by default (you can add a selectInput if you want)
    poly <- sf::read_sf(path, layer = layers[1])
    return(poly)
  }
  
  NULL
}


# helper2: Convert drawn GeoJSON polygon to sf geometry
geojson_feature_to_sfc <- function(feature, crs = 4326) {
  if (is.null(feature) || is.null(feature$geometry$type)) return(NULL)
  
  typ <- feature$geometry$type
  coords <- feature$geometry$coordinates
  
  #  POLYGON / MULTIPOLYGON 
  if (typ == "Polygon" || typ == "MultiPolygon") {
    ring <- if (typ == "Polygon") coords[[1]] else coords[[1]][[1]]
    m <- do.call(rbind, lapply(ring, function(p) c(p[[1]], p[[2]])))
    return(sf::st_sfc(sf::st_polygon(list(m)), crs = crs))
  }
  
  # CIRCLE: Point + radius (meters)
  if (typ == "Point" && !is.null(feature$properties$radius)) {
    lon <- coords[[1]]
    lat <- coords[[2]]
    radius_m <- as.numeric(feature$properties$radius)
    
    # choose a local UTM zone for correct meters
    zone <- floor((lon + 180) / 6) + 1
    epsg <- if (lat >= 0) 32600 + zone else 32700 + zone
    
    pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    circle_poly <- pt |>
      sf::st_transform(epsg) |>
      sf::st_buffer(dist = radius_m) |>
      sf::st_transform(4326)
    
    return(circle_poly)
  }
  
  NULL
}
# helper3: popup on points
make_popup <- function(d, track_col) {
  coords <- sf::st_coordinates(d)
  dd <- sf::st_drop_geometry(d)
  tt <- as.character(mt_time(d))
  
  paste0(
    "<b>Track:</b> ", as.character(dd[[track_col]]), "<br>",
    "<b>Time:</b> ", tt, "<br>",
    "<b>Lon:</b> ", round(coords[, 1], 6), "<br>",
    "<b>Lat:</b> ", round(coords[, 2], 6),
    if ("within" %in% names(dd)) paste0("<br><b>Within:</b> ", dd$within) else "",
    if ("area" %in% names(dd) && any(!is.na(dd$area))) paste0("<br><b>Area:</b> ", dd$area) else ""
  )
}


#########################
shinyModuleUserInterface <- function(id, label = NULL, ...) {
  
  ns <- NS(id)
  
  tagList(
    titlePanel("Geofence"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4("Tracks"),
        checkboxGroupInput(ns("animals"), NULL, choices = NULL),
        fluidRow(
          column(6, actionButton(ns("select_all_animals"), "Select All", class = "btn-sm")),
          column(6, actionButton(ns("unselect_animals"), "Unselect", class = "btn-sm"))
        ),
        
        br(),
        
        h4("Polygon Boundary"),
        radioButtons(
          ns("user_polygon"),
          label = "Polygon input method",
          choices = c("Upload ZIP/GPKG" = "upload", "Draw on map" = "draw"),
          selected = "upload"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("user_polygon")),
          fileInput(
            ns("upload_user_polygon"),
            "Choose a polygon file (.zip shapefile or .gpkg):",
            accept = c(".zip", ".gpkg")
          )
        ),
        
        hr(),
        actionButton(ns("apply_btn"), "Apply Changes", class = "btn-primary btn-block"),
        uiOutput(ns("apply_warning")),
        hr(),
        
        h4("Download"),
        downloadButton(ns("save_html"), "Download as HTML", class = "btn-sm"),
        downloadButton(ns("save_png"), "Save Map as PNG", class = "btn-sm")
      ),
      
      mainPanel(
        leafletOutput(ns("leafmap"), height = "85vh"),
        width = 9
      )
    )
  )
}

###server##################

  
shinyModule <- function(input, output, session, data) {
    
    ns <- session$ns
    
    if (is.null(data) || nrow(data) == 0) {
      message("Input is NULL or has 0 rows — returning NULL.")
      return(NULL)
    }
    
    if (!sf::st_is_longlat(data)) data <- sf::st_transform(data, 4326)
    
    track_col_raw <- mt_track_id_column(data)
    data <- data %>%
      mutate(trackId = as.character(mt_track_id(.))) %>%
      group_by(trackId) %>%
      filter(n() >= 2) %>%
      ungroup()
    
    track_col <- "trackId"
    
    
    
    dataObj <- reactive({ data })
    current <- reactiveVal(data)
    locked_settings <- reactiveVal(NULL)
    locked_data <- reactiveVal(NULL)
    
    coords_df <- reactiveVal(NULL)
    
    
    
    track_col <- "trackId"
    
    apply_warning <- reactiveVal(FALSE)
    output$apply_warning <- renderUI({
      if (isTRUE(apply_warning())) {
        logger.info("No track selected.")
        div(style = "color:#b30000; font-weight:800; margin-top:6px;","No track selected")
      } else {
        NULL
      }
    })
    
    
    
    
    ##################################
    if (!sf::st_is_longlat(data)) 
    
    track_col <- "trackId"
    
    
    all_ids <- reactive({
      sort(unique(as.character(data[[track_col]])))
    })
    
    observeEvent(all_ids(), {
      updateCheckboxGroupInput(session, "animals",
                               choices = all_ids(),
                               selected = all_ids())
    }, ignoreInit = FALSE)
    
    observeEvent(input$select_all_animals, {
      updateCheckboxGroupInput(session, "animals", selected = all_ids())
    })
    
    observeEvent(input$unselect_animals, {
      updateCheckboxGroupInput(session, "animals", selected = character(0))
    })
    
    # Filtered data (selected animals)
    selected_data <- reactive({
      req(input$animals)
      d <- data
      d[d[[track_col]] %in% input$animals, ]
    })
    
    # initialize
    observe({
      if (!is.null(locked_data())) return()
      d0 <- selected_data()
      req(nrow(d0) > 0)
      locked_data(d0)
      locked_settings(list(animals = input$animals, select_attr = input$select_attr))
    })
    
    
    # Apply button
    observeEvent(input$apply_btn, {
      # if no track selected, not change map
      if (is.null(input$animals) || length(input$animals) == 0) {
        apply_warning(TRUE)
        return()
      }
      
      apply_warning(FALSE)
      locked_data(selected_data())
      locked_settings(list(animals = input$animals, select_attr = input$select_attr))
    }, ignoreInit = TRUE)
    
    # Lines 
    track_lines <- reactive({
      d <- locked_data()
      req(d)
      req(nrow(d) >= 2)
      mt_track_lines(d)
    })
    
    
    mmap <- reactive({
      d <- locked_data()
      req(d)
      req(nrow(d) >= 1)
      
      bounds <- as.vector(sf::st_bbox(d))
      ids <- sort(unique(as.character(d[[track_col]])))
      pal <- colorFactor(palette = pals::glasbey(), domain = ids)
      
      # colors for points
      d$col <- pal(as.character(d[[track_col]]))
      
      # Build lines
      tl <- track_lines()
      tl$col <- pal(as.character(tl[[track_col]]))
      
      
      s <- locked_settings()
      attr_sel <- if (!is.null(s) && !is.null(s$select_attr)) s$select_attr else character(0)
      
      d$popup_html <- make_popup(d, track_col)
   
    
    #################
    polygon_boundary <- read_sf(paste0(getAppFilePath("polygon"),"polygon_boundary.shp"))
    # polygon_boundary: for now you still read from app files (later: zip upload)
    polygon_boundary <- read_sf(file.path(getAppFilePath("polygon"), "polygon_boundary.shp"))
    
    # crop to current displayed points
    bndry_crop <- st_crop(polygon_boundary, st_bbox(d))
    
    # init
    d$within <- 0L
    d$area <- NA_character_
    
    if (nrow(bndry_crop) > 0) {
      within_list <- st_within(d, bndry_crop)         # list, one element per point
      inside <- lengths(within_list) > 0
      d$within[inside] <- 1L
      
      if ("NAME" %in% names(bndry_crop)) {
        first_poly <- vapply(within_list[inside], function(ix) ix[1], integer(1))
        d$area[inside] <- as.character(bndry_crop$NAME[first_poly])
      }
    }
    d$popup_html <- make_popup(d, track_col)
    ####################################
    
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addScaleBar(position = "topleft") %>%
      addCircleMarkers(data = d,radius = 1, opacity = 0.7, fillOpacity = 0.5, color = ~col, popup = ~popup_html, group = "Points") %>%
      addPolylines( data = tl,  weight = 3,  opacity = 0.5, color = ~col,  group = "Lines") %>%
      addLegend(position = "bottomright", pal = pal,values = ids, title = "Tracks", opacity = 0.8) 
    
    if(ns("user_polygon") == "'Upload'"){
    m <- m %>%  
      addDrawToolbar(
          targetGroup = "drawn",
          polygonOptions = drawPolygonOptions(),
          polylineOptions = FALSE,
          rectangleOptions = TRUE,
          circleOptions = TRUE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions()
        )}
    m <- m %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),overlayGroups = c("Lines", "Points"), options = layersControlOptions(collapsed = FALSE))
    m
  })
  
  # when user draws a new polygon
  observeEvent(input$leafmap_draw_new_feature, {
    f <- input$leafmap_draw_new_feature
    req(f)
    req(identical(f$geometry$type, "Polygon"))
    
    df <- geojson_polygon_to_df(f)
    coords_df(df)
    
    leafletProxy(ns("leafmap"), session) %>%
      clearGroup("drawn") %>%
      addPolygons(lng = df$lng, lat = df$lat, group = "drawn")
  })
  
  observeEvent(input$leafmap_draw_edited_features, {
    feats <- input$leafmap_draw_edited_features$features
    req(feats)
    req(length(feats) > 0)
    
    df <- geojson_polygon_to_df(feats[[1]])
    coords_df(df)
    
    leafletProxy(ns("leafmap"), session) %>%
      clearGroup("drawn") %>%
      addPolygons(lng = df$lng, lat = df$lat, group = "drawn")
  })
  
  # show coordinates in a table
  output$coords_tbl <- renderTable({
    req(coords_df())
    coords_df()
  })
  
  
  
  
  
  output$leafmap <- renderLeaflet(mmap())
  
  
  #### download HTML
  output$save_html <- downloadHandler(
    filename = function() paste0("LeafletMap_", Sys.Date(), ".html"),
    content = function(file) {
      
      show_modal_spinner(spin = "fading-circle", text = "Saving HTML…")
      on.exit(remove_modal_spinner(), add = TRUE)
      
      saveWidget(widget = mmap(), file = file)
    }
  )
  
  #### save map as PNG
  output$save_png <- downloadHandler(
    filename = paste0("LeafletMap_", Sys.Date(),".png"),
    content = function(file) {
      
      show_modal_spinner(spin = "fading-circle", text = "Saving PNG…")
      on.exit(remove_modal_spinner(), add = TRUE)
      
      html_file <- "leaflet_export.html"
      saveWidget(mmap(), file = html_file, selfcontained = TRUE)
      Sys.sleep(2)
      webshot2::webshot(url = html_file, file = file, vwidth = 1000, vheight = 800)})
  
      
      return(reactive({ current() }))
}
    