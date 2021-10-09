# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-10-09"), 
                                    author = "corb", 
                                    proj_name = "hurricane-play", 
                                    script_type = "etl", 
                                    notepad = paste0("dl and compile a dataframe of historical hurricane paths")), 
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
# library(gt)
library(sf)
library(fs)
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

# this is the nhc url where these zip files can be grabbed
nhc_gis_site <- "https://www.nhc.noaa.gov/gis/best_track/"

# create a function that can generate a vector of storm codes for nhc
gen_storm_codes <- function(yr, n_storms) {
  aa <- as.character(seq(1, n_storms))
  bb <- ifelse(str_length(aa) == 1, 
               paste0("0", aa), aa)
  cc <- paste0("al", bb, yr)
  return_me <- cc
  return(return_me)}

# initialize a df object that can be used to generate URLS to pull data
#   look at the url above and add args here to pull in more years
#   you can count the number of storms for each year to generate the
#   right vector of storm IDs
data_packs <- data.frame(datasets = c(gen_storm_codes(2015, 12), 
                                      gen_storm_codes(2016, 16),
                                      gen_storm_codes(2017, 19),
                                      gen_storm_codes(2018, 16),
                                      gen_storm_codes(2019, 19),
                                      gen_storm_codes(2020, 31)))
nrow(data_packs)

# builds a dataframe with all needed file names to download and unzip
data_packs <- data_packs %>% 
  mutate(dataset_zips = paste0(datasets, "_best_track.zip"), 
         dataset_urls = paste0(nhc_gis_site, dataset_zips), 
         zip_paths = paste0(getwd(), "/etl/ore/zipped_", datasets), 
         unzip_paths = paste0(getwd(), "/etl/ore/unzipped_", datasets)) 

# function to download the zipped files from the site and then unzip them
fun_consume <- function(arg1, arg2, arg3) {
  download.file(url = arg1, 
                destfile = arg2)
  unzip(zipfile = arg2, exdir = arg3)}

# performs download from the site and unzips the data
clockin()
pwalk(list(arg1 = data_packs$dataset_urls, 
           arg2 = data_packs$zip_paths, 
           arg3 = data_packs$unzip_paths), 
      fun_consume)
clockout()

# cleanup ??????????????????????????????????????
rm(nhc_gis_site, fun_consume, gen_storm_codes)
ls()
trash()
mem_used()

# ^ -----

# pick out the shape files and prep them for analysis ------------------

# rename so that we can join this file path and directory df to other stuff
data_packs <- data_packs %>% rename(storm_id = datasets)

# find all the different shape files in all the unzips :::::::::::::::::::::
interim <- data.frame(filenames = list.files(data_packs$unzip_paths)) %>% 
  # this block only gives us the .shp files, which we want to read
  mutate(is_dotshp = ifelse(str_ends(filenames, ".shp") == TRUE, 
                            TRUE, FALSE)) %>% 
  filter(is_dotshp == TRUE) %>% select(-is_dotshp) %>% 
  # parse the filenames to help with functions later
  #   there are 4 types of shp files in the zip folders = line, point, 
  #     polygon (radius), and windswath, with different schemas
  mutate(setup_var = str_length(filenames), 
         shape_type = str_sub(filenames, 
                              start = 1, 
                              end = -5L), 
         shape_type = str_sub(shape_type, start = 10, end = -1L), 
         storm_id = str_sub(filenames, start = 1, 8), 
         storm_id = tolower(storm_id)) %>% 
  select(-setup_var)

# join the data_packs df object to the interim so that we have a robust list
#   of all the files we want to read in and where they are in the 
#   directory
interim <- left_join(interim, data_packs, by = "storm_id")
dim(interim)

interim <- interim %>% 
  mutate(st_read_path = paste0(unzip_paths, "/", filenames))

# build a reader that will read in and combine the shape files
fun_shp_consumer <- function(path_arg, strmid_arg) {
  aa <- st_read(dsn = path_arg)
  aa <- aa %>% mutate(storm_id = as.character(strmid_arg))}

# tests +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# fun_shp_consumer(interim$st_read_path[4],
#                  interim$storm_id[4]) %>% 
#   ggplot() + geom_sf(aes(color = as.factor(RADII)))
# tests +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# consume and combine the line shape files ::::::::::::::::::::::::::::::::
interim2 <- interim %>% filter(shape_type == "lin")

clockin()
dfsf_lin <- pmap_dfr(list(interim2$st_read_path, interim2$storm_id), 
                     fun_shp_consumer)
rm(interim2)
clockout()

dim(dfsf_lin)
length(unique(dfsf_lin$storm_id))

# consume and combine the points shape files :::::::::::::::::::::::::::::::
interim2 <- interim %>% filter(shape_type == "pts")

clockin()
dfsf_pts <- pmap_dfr(list(interim2$st_read_path, interim2$storm_id), 
                     fun_shp_consumer)
rm(interim2)
clockout()

dim(dfsf_pts)
length(unique(dfsf_pts$storm_id))

# consume and combine the radius shape files :::::::::::::::::::::::::::::::
interim2 <- interim %>% filter(shape_type == "radii")

clockin()
dfsf_radi <- pmap_dfr(list(interim2$st_read_path, interim2$storm_id), 
                     fun_shp_consumer)
rm(interim2)
clockout()

dim(dfsf_radi)
length(unique(dfsf_radi$storm_id))

# consume and combine the radius shape files :::::::::::::::::::::::::::::::
interim2 <- interim %>% filter(shape_type == "windswath")

clockin()
dfsf_wind <- pmap_dfr(list(interim2$st_read_path, interim2$storm_id), 
                      fun_shp_consumer)
rm(interim2)
clockout()

dim(dfsf_wind)
length(unique(dfsf_wind$storm_id))

# cleanup ???????????????????????????????????????
rm(interim, fun_shp_consumer)
ls()
trash()
mem_used()
sizer(c(dfsf_lin, dfsf_pts, dfsf_radi, dfsf_wind))
obj_size(c(dfsf_lin, dfsf_pts, dfsf_radi, dfsf_wind))

# ^ -----

# delete all those raw file downloads ----------------------------- 

walk(data_packs$unzip_paths, dir_delete)
walk(data_packs$zip_paths, file_delete)

# cleanup ??????????????????????????
trash()
ls()
mem_used()

# ^ -----

# clean up and meta analyze the shape files -----------------------------

# create a meta df that combines intel from each type of shape file
df_storms <- data.frame(storm_id = unique(dfsf_lin$storm_id))

df_storms <- df_storms %>% as_tibble() %>% 
  mutate(storm_year = str_sub(storm_id, start = 5), 
         storm_yr_num = str_sub(storm_id, start = 3, end = 4), 
         storm_year = as.double(storm_year), 
         storm_yr_num = as.double(storm_yr_num), 
         etl_df_creation = Sys.time())

# pull if storm was a hurricane and the category of the hurricane
interim <- dfsf_lin %>% as_tibble() %>% select(-geometry) %>% 
  rename(hur_severity = SS) %>% group_by(storm_id) %>% 
  summarise(max_hur_sev = max(hur_severity)) %>% 
  mutate(hur_ind = ifelse(max_hur_sev > 0, TRUE, FALSE))

df_storms <- left_join(df_storms, interim, by = "storm_id")
rm(interim)

# extract storm insights from the points shape file
interim <- dfsf_pts %>% as_tibble() %>% select(-geometry) %>% 
  group_by(storm_id) %>% 
  summarise(min_time = min(DTG), 
            max_time = max(DTG), 
            max_intensity = max(INTENSITY))

df_storms <- left_join(df_storms, interim, by = "storm_id")
rm(interim)

# trying to clean up and get the real storm name but will revisit this !!!
# dfsf_pts %>% as_tibble() %>% select(-geometry) %>% 
#   mutate(delete_me = ifelse(STORMNAME %in% c('INVEST', 'TEST', 
#                                              'ONE', 'TWO', 'THREE', 
#                                              'FOUR'), 
#                             TRUE, FALSE)) %>% 
#   mutate(delete_me = ifelse(str_detect(STORMNAME, 'GENESIS') == TRUE, 
#                             TRUE, delete_me)) %>% 
#   mutate(delete_me = ifelse(is.na(STORMNAME), TRUE, delete_me)) %>% 
#   filter(delete_me == FALSE) %>% 
#   group_by(storm_id, STORMNAME, delete_me) %>% 
#   summarise(storm_name = min(STORMNAME), 
#             min_time = min(DTG), 
#             max_time = max(DTG), 
#             max_intensity = max(INTENSITY)) 

# cleanups on the shape file dataframes :::::::::::::::::::::::::::::::
dfsf_lin <- dfsf_lin %>% rename(hur_severity = SS)

# cleanup ?????????????????????????????????????????????????????????
ls()
trash()
mem_used()

# ^ -----

# write final shape files to clean location for further analysis ---------

# note: getting some errors when writing the shape files, look into this 
#   futher, opened issue #3 to address this
write_sf(dfsf_lin, paste0(getwd(), "/etl/ingot/", "dfsf_lin", ".shp"))
write_sf(dfsf_pts, paste0(getwd(), "/etl/ingot/", "dfsf_pts", ".shp"))
write_sf(dfsf_radi, paste0(getwd(), "/etl/ingot/", "dfsf_radi", ".shp"))
write_sf(dfsf_wind, paste0(getwd(), "/etl/ingot/", "dfsf_wind", ".shp"))
write_rds(df_storms, paste0(getwd(), "/etl/ingot/", "df_storms", ".rds"))

# ^ -----