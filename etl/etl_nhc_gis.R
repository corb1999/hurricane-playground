# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-09-26"), 
                                    author = "corb", 
                                    proj_name = "hurricane-play", 
                                    script_type = "etl", 
                                    notepad = paste0("load and prep nhc gis data")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(gt)
library(sf)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5)
mem_used()

# basic helper functions ***************************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# pull in basic shp files from nhc ------------------------------------------

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# which storm do you want to pull data for? type here
storm_designation <- "al182021"
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# this is the nhs url where these zip files can be grabbed
nhc_gis_site <- "https://www.nhc.noaa.gov/gis/forecast/archive/"

# list the different shp file packages you are interested in
data_packs <- data.frame(datasets = c("5day_latest", 
                                      "fcst_latest", 
                                      "best_track"))

# builds a dataframe with all needed file names to download and unzip
data_packs <- data_packs %>% 
  mutate(dataset_zips = paste0("_", datasets, ".zip"), 
         dataset_urls = paste0(nhc_gis_site, storm_designation, 
                               dataset_zips), 
         zip_paths = paste0(getwd(), "/etl/ore/zipped_", datasets), 
         unzip_paths = paste0(getwd(), "/etl/ore/unzipped_", datasets)) %>% 
  # have to input a custom URL here because it is not in same spot
  mutate(dataset_urls = ifelse(datasets == "best_track", 
                               paste0("https://www.nhc.noaa.gov/gis/best_track/", 
                                      storm_designation, 
                                      dataset_zips), 
                               dataset_urls))

# function to download the zipped files from the site and then unzip them
fun_consume <- function(arg1, arg2, arg3) {
  download.file(url = arg1, 
                destfile = arg2)
  unzip(zipfile = arg2, exdir = arg3)}

# performs download from the site and unzips the data
pwalk(list(arg1 = data_packs$dataset_urls, 
           arg2 = data_packs$zip_paths, 
           arg3 = data_packs$unzip_paths), 
      fun_consume)

# ^ -----
