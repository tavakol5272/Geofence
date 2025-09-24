library('move2')
library(sf)
library(ggplot2)
library(tidyverse)

 data <- readRDS("./data/raw/Yahatinda_move2.rds")
# 
  
rFunction = function(data, polygon = NULL) {
  
  polygon_boundary <- read_sf(paste0(getAppFilePath("polygon"),"polygon_boundary.shp"))
  
  ##transform the data into data-frame and sf files
  coords <- sf::st_coordinates(data)
  original_track_id_column <- mt_track_id_column(data)

  data_df <- data |>
    mutate(
      location_long = coords[, 1],
      location_lat = coords[, 2],
      trackId = original_track_id_column
    ) |>
    as.data.frame()

  #data_df <-as.data.frame(data)
  data_sf <- st_as_sf(data_df, coords = c("location_long", "location_lat"), 
  crs = st_crs(4326))
  
  trck_data <- data_sf %>% 
    group_by(trackId) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  bndry_crop <-st_crop(polygon_boundary, data_sf) ## crop the area boundaries wrt the dataset
  ###check if there are no protected area
  
  if (nrow(bndry_crop)==0) 
  {
    logger.info("There is no intersection of your track(s) with any polygon in the Y2Y region. Returning input data set.")
    final_dat <- data
  } else
  {
  #bndry_crop1 <- bndry |> filter( PA_NAME %in% bndry_crop$PA_NAME)
  cent_df <-st_centroid(bndry_crop)
 
  
  ### filter which points are within the protected area polygon or not
  dt <- as.matrix(st_within( data_sf, bndry_crop))
  dat <-data.frame(column = col(dt)[dt], row = row(dt)[dt]) ###extract the index of points inside the polygon
  data_df$within <-0
  data_df$within[dat[,2]] <-1
  data_df$area <- ifelse(data_df$within ==1, bndry_crop$NAME[dat[,1]], NA) ## Include the area name in the dataset
  
  data_sf$within <-0      
  data_sf$within[dat[,2]] <-1
  
  ### Plot the points and check if the assignment is correct
  pa_data <-ggplot()+
    geom_sf(data = bndry_crop, fill= "grey")+
    geom_sf(data = trck_data, size = 0.5)+
    geom_sf(data = data_sf, size=0.5, aes(col= as.factor(within)), alpha = 0.7)+
    geom_sf_text(data = cent_df, aes(label = NAME))+
    labs(y = "Latitude", x = "Longitude", col = "Points\nInside/Outside")+
    scale_color_discrete(labels = c("Outside", "Inside"))+
    theme_bw()
  
  #pa_data
  
  ## Save the output in csv and jpeg 
  ggsave(pa_data, filename = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"PA_locations.jpeg"), 
         height = 6, width = 9, units = "in", dpi = 300)
  
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
 }
