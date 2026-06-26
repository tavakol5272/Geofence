# Geofence

MoveApps

Github repository: *https://github.com/nilanjanchatterjee/Geofence*

## Description
This app classifies animal tracking locations from a move2 object as inside or outside a user-defined polygon boundary. 
Users can upload the boundary as either a zipped shapefile (.zip) or a GeoPackage (.gpkg). 
The app returns the input tracking data with an added within column and generates a plot for checking the result.

## Documentation
This app is used to geofence animal tracking data from a move2 object. 
The user uploads a polygon boundary in either .zip shapefile or .gpkg format. 
The app then checks each tracking location against the uploaded boundary and classifies it as inside or outside then returns the output dataset with an additional within column(inside, outside) 
and creates two files; table in CSV and a plot in PNG file showing the uploaded boundary and inside/outside classification.

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

* **flagged_points (.csv):** containing track ID, coordinates, timestamp, flag status(inside/outside), and polygon shape ID.
* **geofence_check (.png):** showing the uploaded boundary, track lines, point locations, and inside/outside classification
The QC plot helps users visually confirm whether the geofence classification is correct.

### Settings

"Polygon Boundary": upload a polygon file in .zip shapefile or .gpkg format in setting.

"Flag Points": Applies the geofence to the currently selected tracks and classifies locations as inside or outside the chosen boundary.

### Changes in output data

The output data contain the full original input data with an additional `within` column. This column stores the geofence result for each point (`1` = inside, `0` = outside). 
track selection only affects the map display.

### Most common errors

**Unsupported upload:** Only `.zip` shapefiles and `.gpkg` files with polygon geometry are supported.  

### Null or error handling

**No polygon file found:** the input data are returned unchanged
**Invalid file type:** the input data are returned unchanged
**No valid polygon boundary found:** the input data are returned unchanged
**Invalid boundary geometry:** the input data are returned unchanged