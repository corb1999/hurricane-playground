# hurricane-playground
R scripts and analysis of NHC data using sf package

Getting data from here: https://www.nhc.noaa.gov/gis/

# contents...
- etl: the nhc_gis.R script will take a given storm ID, like 'al012021' and will download the zip files of the storm forecast from the nhc website and then make them available in the R environment for visualization. separately the nhc_historical_besttracks.R script does a similar action but pulls down and combines historical storm shape files
- eda: the eda files are designed to house analysis and visuals. the basic eda script will invoke the etl script to pull the latest forecast of a current storm and quickly visualize it

