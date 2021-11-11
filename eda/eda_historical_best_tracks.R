# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-11-10"), 
                                    author = "corb", 
                                    proj_name = "hurricane", 
                                    script_type = "eda", 
                                    notepad = paste0("historical best tracks")), 
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

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load data -----------------------------------------------------

# load a rds file
loader_path1 <- paste0(getwd(), "/etl/ingot/df_storms.rds")
clockin()
raw_df <- readRDS(loader_path1)
clockout()
dim(raw_df)
colnames(raw_df)

df_storms <- raw_df

# load the best tracks shape file
loader_path1 <- paste0(getwd(), "/etl/ingot/dfsf_lin")
clockin()
dfsf_lin <- st_read(dsn = loader_path1)
clockout()
head(dfsf_lin)

dfsf_lin <- dfsf_lin %>% 
  rename(storm_id = storm_d)

dfsf_lin <- left_join(dfsf_lin, df_storms, by = "storm_id")

# load a state shapefile ::::::::::::::::::::::::::::::::::::::::::
loader_path1 <- paste0(getwd(), "/etl/tl_2020_us_state")
clockin()
dfsf_state <- st_read(dsn = loader_path1)
clockout()
head(dfsf_state)

dfsf_state <- dfsf_state %>% 
  filter(STUSPS %in% c("TX", "LA", "MS", "AL", "FL", "GA", 
                       "SC", "NC", "VA", "MD", "DE", "NJ", 
                       "NY", "CT", "RI", "MA", "PA", "ME"))

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!!!!!
rm(raw_df, loader_path1)
ls()
trash()
mem_used()

# ^ -----

# plot settings -------------------------------------------------------

plt_dfsf_state <- dfsf_state %>% 
  # filter(STUSPS %in% c('NC', 'SC')) %>% 
  filter(REGION > 0)


plt_dfsf_lin <- dfsf_lin %>% 
  # filter(hur_ind == TRUE) %>% 
  filter(storm_year < 2021) %>% 
  filter(storm_yr_num >= 1)

# ^ -----

# test visual/map ---------------------------------------------

ggplot() + 
  geom_sf(data = plt_dfsf_state, fill = NA, 
          size = 0.7, color = "black") + 
  geom_sf(aes(color = as.factor(max_hur_sev), 
              size = as.factor(max_hur_sev)), 
          data = plt_dfsf_lin) + 
  scale_color_manual(values = c('grey', '#FBD148', 
                                '#F9975D', 'orange', 
                                '#C85C5C', 'red')) + 
  scale_size_manual(values = seq(from = 0.8, to = 2, 
                                 length.out = 6), 
                    guide = 'none') + 
  facet_wrap(vars(storm_year)) + 
  theme_minimal() + theme(legend.position = 'top') + 
  # lims(x = c(-85, -65), y = c(30, 40)) +
  labs(color = 'HUR Category')

# ^ -----