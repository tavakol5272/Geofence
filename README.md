# Geofence

MoveApps

Github repository: *https://github.com/nilanjanchatterjee/Geofence*

## Description
The app classifies animal tracking locations as inside or outside a user-defined polygon boundary. 
The boundary can either be drawn directly on the interactive map or uploaded as a polygon file in .zip shapefile or .gpkg format.
The app is intended for interactive geofencing of movement data, allowing users to identify and visualize which animal locations fall inside or outside a selected boundary.

## Documentation
This app is used to geofence animal tracking data from a move2 object. Users can select one or more tracks, then define a polygon boundary either by drawing it on the map or by uploading a .zip shapefile or .gpkg file. 
After clicking Flag Points, the app classifies the selected locations as inside or outside the boundary and displays the results on the map.
The map shows track lines, point locations, and popups with time, coordinates, and flag status. 
Users can download the drawn boundary as a GeoPackage, export the flagged points as a CSV file, and save the map as HTML or PNG.
If users want, they can also draw a polygon, download it with , and use it again in a later run by uploading it as a boundary file.

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. 

#### Required data properties
The App should work for any kind of (location) data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`


### Artefacts

The app can generate the following artefacts for download:

* **Drawn boundary (.gpkg):** exports the polygon drawn directly in the app as a GeoPackage file. This can be used again in later runs by uploading it as a boundary file.
* **Flagged data (.csv):** exports the selected animal locations together with longitude, latitude, timestamp, and their classification as inside or outside the boundary.
* **Map (.html):** saves the current interactive map as an HTML file.
* **Map (.png):** saves the current map as a static PNG image.


### Settings
"Tracks": Select one or more individuals to display on the map. Buttons are available to select all or unselect all tracks.

"Polygon Boundary": Choose how the boundary should be defined. Users can either draw an area directly on the map or upload a polygon file in .zip shapefile or .gpkg format.

"Flag Points": Applies the geofence to the currently selected tracks and classifies locations as inside or outside the chosen boundary.

"Download":
Download HTML: locally downloads the current map in HTML format.
Download PNG: locally downloads the current map in PNG format.
Download Drawn boundary (.gpkg): downloads the polygon drawn in the app as a GeoPackage file. This option is available only when the draw mode is used.
Flagged data: downloads the current flagged points as a CSV file.

### Changes in output data

The input data remain unchanged and are passed on as output. Geofence results are only used for visualization and downloadable exports.

### Most common errors

**Unsupported upload:** Only `.zip` shapefiles and `.gpkg` files with polygon geometry are supported.
**No track selected:** The map is not updated until at least one track is selected.
**Boundary changed:** If tracks or boundary are changed, users need to click **Flag Points** again.

### Null or error handling

**Big data:** If the input dataset exceeds 200,000 locations, the Shiny UI may not perform properly.
**No track selected:** The map is not updated until at least one track is chosen.
**No valid boundary:** If no valid boundary is drawn or uploaded, no geofence classification is applied.
