# Interactive Geofence

MoveApps

Github repository: https://github.com/tavakol5272/Geofence

## Description
Flags animal locations as inside or outside one or more polygon boundaries, drawn directly on an interactive map or uploaded as a .zip shapefile or .gpkg file. The flagged data can be passed on with flag columns added, or filtered to only the locations inside or outside the boundary.

## Documentation
This App geofences animal tracking data. The user defines a polygon boundary either by drawing polygons, rectangles or circles on the interactive map, or by uploading a polygon file (.zip shapefile or .gpkg). A polygon file can be uploaded in the App itself, or provided as the `Polygon boundary file` setting when the Workflow is configured; the file uploaded in the App takes precedence.

Clicking `Flag Points` classifies every location of all tracks as inside or outside the boundary and records which polygon(s) contain it. A location exactly on the border of a polygon counts as outside. When the App is opened, the stored settings (selected tracks, drawn shapes or uploaded file, output option) are applied automatically, so a Workflow run with stored settings returns the flagged data without opening the App.

The map shows the selected tracks as lines and their locations as points in the track colour. Locations inside the boundary have a black border, locations outside a gold border. Popups show the track, time (UTC), coordinates, flag and polygon number. Uploaded boundaries are shown in red. The `Summary` tab lists per selected track the number of locations inside and outside the boundary.

The map (HTML or PNG), the drawn boundary (GeoPackage) and the flagged locations of the selected tracks (CSV) can be downloaded from the App. A drawn boundary saved as GeoPackage can be uploaded again in a later run.

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group.

#### Required data properties
The App should work for any kind of location data in any coordinate reference system.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
The App does not create artefacts. The following files can be downloaded locally from the App UI:

`Geofence_map_<date>.html`: interactive map of the selected tracks, flagged locations and boundary.  
`Geofence_map_<date>.png`: static image of the same map.  
`drawn_boundary_<date>.gpkg`: this button is only shown when `Draw area on map` is selected as `Boundary source`. It downloads the shapes drawn on the map as a GeoPackage (see Artefacts), which can be uploaded again with `Upload area file` in a later run.
`flagged_points_<date>.csv`: locations of the selected tracks with the columns `track_id`, `longitude`, `latitude`, `timestamp` (UTC), `flag` (`inside`, `outside` or `-` if not flagged) and `shape_id`.

### Settings
**Settings in the App UI:**

`Tracks` (animals): select the tracks shown on the map and used for the summary and CSV download. `Select All` and `Unselect All` select or unselect all tracks. Default: all tracks. The track selection does not affect the data passed on to the next App.

`Boundary source` (boundary_method): `Draw area on map` to draw polygons, rectangles or circles with the drawing toolbar on the map, or `Upload area file` to use a polygon file. Default: `Draw area on map`. Drawn shapes are kept by `Store settings` and shown again on the map when the App is reopened.

`Choose polygon file (.zip shapefile or .gpkg)` (upload_user_polygon): only shown for `Upload area file`. A zipped shapefile (the first `.shp` in the zip is used) or a GeoPackage (the first layer with polygons is used). The uploaded file is kept by `Store settings`. If no file is uploaded here, the `Polygon boundary file` of the App settings is used.

`Data passed on to the next App` (output_mode): `All locations, with flag columns` (default), `Only locations inside the boundary` or `Only locations outside the boundary`.

`Flag Points`: applies the selected tracks, boundary and output option. Changes take effect only after clicking this button.

`Download map as HTML`, `Download map as PNG`, `Download drawn boundary (.gpkg)` (only for `Draw area on map`) and `Download selected flagged data CSV`: see Artefacts.

`Store settings`: click to store the current settings of the App for future Workflow runs. 

`Restore default settings`: click to restore to the default setting values of the App.

**Setting in the App configuration:**

`Polygon boundary file` (boundary_file): optional. A .zip shapefile or a .gpkg with polygons, uploaded when configuring the Workflow. It is only used when `Upload area file` is selected and no file is uploaded in the App. To use it in Workflow runs, open the App once, select `Upload area file` and click `Store settings`.

### Changes in output data
The App adds to the input data the columns:

`within`: `1` if the location is inside the boundary, `0` if outside, `NA` if no boundary was defined or the location has no coordinates.  
`shape_id`: number(s) of the polygon(s) that contain the location, separated by commas if polygons overlap (e.g. `1,3`); empty if outside, `NA` if not flagged. Polygons are numbered in drawing order or in the row order of the uploaded file.

With `All locations, with flag columns`, all locations of all tracks are returned. With `Only locations inside/outside the boundary`, only the locations with `within` = `1` or `0` are returned; locations without coordinates are removed. The coordinate reference system of the input data is kept.

### Errors and null handling
**Input data:** If the input data are NULL or have 0 rows, the App returns NULL. If the input has no coordinate reference system, longitude/latitude (EPSG:4326) is assumed with a warning in the log.

**Existing columns:** If the input data already contain a column `within` or `shape_id`, it is overwritten and a warning is written to the log.

**Unsupported upload:** Only .zip shapefiles and .gpkg files with polygon geometry are supported. Otherwise a red message is shown under the upload and no boundary is used.

**No boundary:** If no boundary is drawn or uploaded, the locations are not flagged (`within` and `shape_id` are `NA`) and the data are passed on unfiltered, also if a filter option is selected.

**No track selected:** `Flag Points` shows a red message and nothing is applied.

**No locations left:** If no locations remain after filtering, the App returns NULL with a warning in the log.

**Big data:** With very large datasets (e.g. more than 200,000 locations) the map may respond slowly.

### Technical details
- Locations are transformed to EPSG:4326 for flagging and display only; the returned data keep their original projection.
- The point-in-polygon test uses `sf::st_within()` on all locations at once, computed once per click on `Flag Points`; invalid polygon geometries are repaired with `sf::st_make_valid()`.
- Circles drawn on the map are converted to polygons by buffering the centre point in its UTM zone.
- Drawn shapes are stored as GeoJSON in a hidden input