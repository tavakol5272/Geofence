library(move2)
library(lubridate)
library(sf)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(webshot2)


rFunction = function(data, y2y= c("yes","no"), ...) {
  
  if (is.null(data) || nrow(data) == 0) {
    message("Input is NULL or has 0 rows.")
    return(NULL)
  }
  
  poly_zip <- getAuxiliaryFilePath("polygon")
  
  if (is.null(poly_path)) {
    if (y2y=="yes"){
      polygon_boundary <- read_sf(paste0(getAppFilePath("polygon"),"polygon_boundary.shp"))
    } else {
      logger.info("No polygon uploaded. Upload a polygon ZIP, or set the fallback option to 'yes'.")
      return(data)
    }
      
  } 

  ##transform the data into data-frame and sf files
  #original_track_id_column <- mt_track_id_column(data)
  
  
  data <- sf::st_transform(data, 4326)
  
  data_filtered <- data %>%
    group_by(mt_track_id())%>%
    filter(n() >= 2) %>%
    ungroup()

  track_lines = mt_track_lines(data_filtered)
  
  coords <- sf::st_coordinates(data_filtered)
  
  track_col <- mt_track_id_column(data_filtered)
  track_id <- as.character(mt_track_id(data_filtered))
  
  data_df <- data_filtered %>%
    mutate(
      loc_long = coords[, 1],
      loc_lat  = coords[, 2],
      trackId  = track_id      
    ) %>%
    sf::st_drop_geometry() %>%
    as.data.frame()
  
  data_sf <- st_as_sf(data_df, coords = c("location_long", "location_lat"))
  bndry_crop <-st_crop(polygon_boundary, data_sf) 
  if (nrow(bndry_crop)==0) 
  {
    logger.info("There is no intersection of your track(s) with any polygon in the Y2Y region. Returning input data set.")
    final_dat <- data
  } else
  {
    cent_df <-st_centroid(bndry_crop)
    
    
    ### filter which points are within the protected area polygon or not
    dt <- as.matrix(st_within( data_sf, bndry_crop))
    dat <-data.frame(column = col(dt)[dt], row = row(dt)[dt]) 
    data_df$within <-0
    data_df$within[dat[,2]] <-1
    data_df$area <- ifelse(data_df$within ==1, bndry_crop$NAME[dat[,1]], NA) 
    
    data_sf$within <-0      
    data_sf$within[dat[,2]] <-1
    
    
  
  plot_data <- leaflet(options = leafletOptions(minZoom = 2)) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
    addScaleBar(position = "topleft") %>%
    addCircleMarkers(data = data_filtered,radius = 1, opacity = 0.7, fillOpacity = 0.5, color = ~col, popup = ~popup_html, group = "Points") %>%
    addPolylines( data = data_filtered,  weight = 3,  opacity = 0.5, color = ~col,  group = "Lines") %>%
    addLegend(position = "bottomright", pal = pal,values = ids, title = "Tracks", opacity = 0.8) %>%
    addLayersControl(baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),overlayGroups = c("Lines", "Points"), options = layersControlOptions(collapsed = FALSE))
  
  
    ## Save the output in csv and jpeg 
    ggsave(pa_data, filename = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"PA_locations.jpeg"), 
           height = 6, width = 9, units = "in", dpi = 300)
    
    saveWidget(plot_data, file = "PA_locations.html", selfcontained = TRUE)
    webshot2::webshot("PA_locations.html", file = "PA_locations.png", vwidth = 1400, vheight = 900)
    
    ## converts the data-frame to movestack
    mt_as_move2(
      data_df,
      coords = c("location_long", "location_lat"),
      time_column = "timestamp",
      crs = 4326,
      track_id_column = original_track_id_column,
      track_attributes = names(track_attributes)
    )
    
    ##    final_dat <- move(x= as.numeric(data_df$location.long), y = as.numeric(data_df$location.lat),
    ##                   time = as.POSIXct(data_df$timestamp, format = "%Y-%m-%d %H:%M:%S"),
    ##                    data = data_df, proj = CRS("+proj=longlat +ellps=WGS84"), 
    ##                    animal = data_df$trackId)
  }
  return(final_dat)
  
  #return(result)
}
