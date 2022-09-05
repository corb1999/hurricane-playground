# hurricane-playground
R scripts and analysis of NHC data using sf package

Getting data from here: https://www.nhc.noaa.gov/gis/

# contents...
- etl: the nhc_gis.R script will take a given storm ID, like 'al012021' and will download the zip files of the storm forecast from the nhc website and then make them available in the R environment for visualization. separately the nhc_historical_besttracks.R script does a similar action but pulls down and combines historical storm shape files
- eda: the eda files are designed to house analysis and visuals. the basic eda script will invoke the etl script to pull the latest forecast of a current storm and quickly visualize it. the other eda script visualizes and analyzes the historical hurrican data

# single storm tracker datasets
- dfsf_5day_lin
  - Single record linestring sf object which is a polygon showing the predicted path of the storm. The object also includes the name of the storm (IE, Andrew), and the type (Depression, Tropical Storm, Hurricane)
- dfsf_bt_lin
  - A sf object of linestrings that gives the best measurement of the past track of the storm so far. There may be multiple line strings with different STORMTYPEs
- dfsf_bt_radii
  - Polygon sf object of the best measure of the historical radius of the storm at different snapshots in time (SYNOPTIME). Snapshots are given in 6hr intervals
- dfsf_bt_wsaw
- dfsf_fcast_radi
  - Polygons showing the forcasted shape and breadth of the storm. Different shapes for different radii. Forecasts are typically at 6 hour intervals (VALIDTIME)
- dfsf_init_radii
  - Immediate upcoming shape of the storm. Typically one length sf object polygon
