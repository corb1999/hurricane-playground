# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-09-26"), 
                                    author = "corb", 
                                    proj_name = "hurricane-play", 
                                    script_type = "eda", 
                                    notepad = paste0("simple eda viz")), 
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

# run etl and source hurrican data -----------------------------------

# Manually type in the storm ID you want to observe and analyze
storm_designation <- "al182021"

# run the etl to pull the data, source relevant files
sourcerpath <- paste0(getwd(), '/etl/etl_nhc_gis.R')
clockin()
source(file = sourcerpath)
clockout()

# cleanup ????????????????????????????????
ls()
trash()
mem_used()

# ^ -----

# read in outlines of the usa states ---------------------------

# load a state shapefile ::::::::::::::::::::::::::::::::::::::::::
loader_path1 <- paste0(getwd(), "/etl/tl_2020_us_state")
clockin()
dfsf_state <- st_read(dsn = loader_path1)
clockout()
head(dfsf_state)

dfsf_state <- dfsf_state %>% 
  filter(STUSPS %in% c("TX", "LA", "MS", "AL", "FL", "GA", 
                       "SC", "NC", "VA", "MD", "DE", "NJ", 
                       "NY", "CT", "RI", "MA"))

# ^ -----

# generate some basic maps ------------------------------------

(advisory_text <- paste0(dfsf_5day_lin$STORMTYPE, " ", 
                         dfsf_5day_lin$STORMNAME, " as of UTC ", 
                         max(dfsf_bt_radi$SYNOPTIME)))

ggplot() + 
  geom_sf(data = dfsf_state, fill = NA, size = 0.7, color = "black") + 
  geom_sf(data = dfsf_init_radi, fill = 'black', alpha = 0.8) +
  geom_sf(data = dfsf_fcast_radi, aes(fill = VALIDTIME, alpha = RADII)) +
  geom_sf(data = dfsf_bt_wsaw, alpha = 0.25) +
  geom_sf(data = dfsf_bt_radi, alpha = 0.25) +
  geom_sf(data = dfsf_5day_lin, size = 1, linetype = 2, color = "red") +
  geom_sf(data = dfsf_bt_lin, size = 1) + 
  guides(alpha = 'none') + 
  theme_minimal() + theme(legend.position = "none") + 
  labs(title = advisory_text)

# ^ -----
