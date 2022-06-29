#####################
## Train automated identification model for Mariana Fruit Dove songs
## in sound recordings
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 24 June 2022
## last modified: 29 June 2022
######################

library(wgutil)
library(Hmisc)
library(tidyverse)
library(lubridate)

setwd("~/Documents/Data_Analysis/bird_song/Mariana_Fruit_Dove_song/")

# read in sound species annotations from Alexander
a_dat <- data.frame(read_csv("../data/ARU_recording_annotation_Alex_29June2022.csv", 
                             na = c("NA", "N/A", "")))

# drop an extra column from Alex's data
a_dat <- a_dat[, -which(colnames(a_dat) == "...26")]

# tidy column names
colnames(a_dat) <- gsub(":|-|\\.", "_", colnames(a_dat))

# make sound segment identifier
a_dat$seg_id <- paste(a_dat$filename, a_dat$sec_interval_hh_mm_ss, sep = "_")
a_dat$seg_id <- gsub("-.*", "", a_dat$seg_id)

# make observer ID
a_dat$observer <- "AT"

# drop Alex's duplicate Kalabera data
a_dat <- a_dat[-c(1:29), ]

# convert "x" marks to TRUE
a_dat[, 3:20] <- !is.na(a_dat[, 3:20]) 
### end load Alex's data


### load Margo's data
mq <- read.csv("../data/Margo_Fruit_Dove_Sound_Data_29June2022.csv", 
               stringsAsFactors =  FALSE)

# remove example data and duplicate data
mq <- mq[-which(mq$notes == "Example data from Jaden"), ]
mq <- mq[-which(grepl("This was measured before.*", mq$notes)), ]

colnames(mq)[colnames(mq) == "Filename"] <- "filename"

# drop rows with no measurable song
mq <- mq[!is.na(mq$filename), ]
mq <- mq[mq$filename != "", ]
mq <- mq[!is.na(mq$start_time_of_song_hh.mm.ss), ]
mq <- mq[mq$start_time_of_song_hh.mm.ss != "", ]

# remove whitespace from end of filenames
mq$filename <- gsub(" *", "", mq$filename)

# Identify what 30 second segment of the recording each song is in
mq$sec_interval <- gsub("..$", "", mq$start_time_of_song_hh.mm.ss)
mq$seconds <- gsub("^.:..:", "", mq$start_time_of_song_hh.mm.ss)
mq$seconds <- as.integer(as.character(mq$seconds))
# bin seconds to the first or second thirty seconds of each minute
mq$seconds[mq$seconds < 30] <- 0
mq$seconds[mq$seconds >= 30] <- 30

# join seconds to hours and minutes to get song segments
mq$sec_interval <- paste0(mq$sec_interval, as.character(mq$seconds))
# add the last zero to character strings for 00 seconds
mq$sec_interval <- gsub(":0$", ":00", mq$sec_interval)
# add the first zero to character strings for hours
mq$sec_interval <- gsub("^", "0", mq$sec_interval)

# make sound segment identifier
mq$seg_id <- paste(mq$filename, mq$sec_interval, sep = "_")

# make a MAFD column in Margo's data to indicate presence of MAFD song
mq$MAFD <- TRUE # all TRUE b/c margo was measuring songs
########### end load data


## make a df for Mariana Fruit Dove data
mafd_df <- a_dat[, which(colnames(a_dat) %in% 
                           c("filename", "seg_id", 
                             "MAFD"))]
mafd_df <- bind_rows(mafd_df, mq[, which(colnames(mq) %in% 
                                           c("filename", "seg_id", 
                                             "MAFD"))])
table(mafd_df$filename)
summary(as.numeric(table(mafd_df$seg_id)))
table(mafd_df$MAFD)

#################################
## Match acoustic index values to Alex's annotated minutes
# set location of Towsey index data
forest_traj_data_location <- "~/Documents/Saipan_ecology/haldre_forest_trajectories/forest trajectories/data/raw/SoundRecorder/IndicesOutput/Towsey.Acoustic/"
aguiguan_aru_data_location <- "~/Documents/Saipan_ecology/data/audio_recordings/IndicesOutput/Towsey.Acoustic/"
acoustic_index_files <- list(ACI = c(), BGN = c(), CVR = c(), DIF = c(), 
                             ENT = c(), EVN = c(), OSC = c(), PMN = c(), 
                             RHZ = c(), RNG = c(), RPS = c(), RVT = c(), 
                             SPT = c(), SUM = c())


for(i in 1:length(acoustic_index_files)) {
  # get file names for all acoustic index .csv files
  ind_name <- names(acoustic_index_files)[i]
  # ...for forest trajectory recordigs
  for_traj_dat <- list.files(forest_traj_data_location)
  for_traj_dat <- for_traj_dat[grepl(
    paste0(".*", ind_name, ".csv"), 
    for_traj_dat)]
  # ...for Aguiguan ARU recordings
  aguiguan_aru_dat <- list.files(aguiguan_aru_data_location)
  aguiguan_aru_dat <- aguiguan_aru_dat[grepl(
    paste0(".*", ind_name, ".csv"), 
    aguiguan_aru_dat)]
  
  ## read in all data for this index...
  # ...for forest trajectory recordings
  for_traj_dat <- lapply(
    for_traj_dat, 
    FUN = function(x, data_location, acoustic_index) {
      dt <- read.csv(paste0(data_location, x), stringsAsFactors = FALSE)
      dt$acoustic_index <- acoustic_index
      # add the recording filename as a column
      dt$filename <- gsub("__Towsey.*", "", x) 
      dt
    }, data_location = forest_traj_data_location, acoustic_index = ind_name)
  # ...for Aguiguan ARU recordings
  aguiguan_aru_dat <- lapply(
    aguiguan_aru_dat, 
    FUN = function(x, data_location, acoustic_index) {
      dt <- read.csv(paste0(data_location, x), stringsAsFactors = FALSE)
      dt$acoustic_index <- acoustic_index
      # add the recording filename as a column
      dt$filename <- gsub("__Towsey.*", "", x) 
      dt
    }, data_location = aguiguan_aru_data_location, acoustic_index = ind_name)
  
  # bind data from all recordings into a single df
  acoustic_index_files[[i]] <- bind_rows(bind_rows(for_traj_dat), 
                                         bind_rows(aguiguan_aru_dat))
  # add a column showing seconds from start of file to start of analyze segment
  acoustic_index_files[[i]]$seg_start_secs <- acoustic_index_files[[i]]$Index*30
  acoustic_index_files[[i]]$sec_interval <- lubridate::seconds_to_period(
    acoustic_index_files[[i]]$seg_start_secs)
  acoustic_index_files[[i]]$sec_interval <- sprintf(
    '%02d:%02d:%02d', acoustic_index_files[[i]]$sec_interval@hour, 
    lubridate::minute(acoustic_index_files[[i]]$sec_interval), 
    lubridate::second(acoustic_index_files[[i]]$sec_interval))
  acoustic_index_files[[i]]$seg_id <- paste(
    acoustic_index_files[[i]]$filename, acoustic_index_files[[i]]$sec_interval, 
    sep = "_")
}

###### look at some preliminary correlations to choose predictor vars.
## each column is an index value for a frequency band.
## 256 freq bands for 24000 Hz
## 24000/256 = 93.75 Hz per frequency band
## MAFD is between 300 and 600 Hz. 
## 300/93.75 = band 3
## 400/93.75 = band 4
## 500/93.75 = band 5
## 600/93.75 = band 6
## TODO: Make sure those columns are the frequency bands we want
warning("We MUST make sure we know what the frequencies of each column are.")

mafd_df <- left_join(mafd_df, acoustic_index_files$ENT[, colnames(
  acoustic_index_files$ACI) %in% c("c000001", "c000002", "c000003", "c000004", 
                                   "c000005", "c000006", "c000010", 
                                   "c0000015", "c000020", "c000200", "seg_id")])

# which column have predictor variables right now? (for quick exploration)
names(mafd_df)
for(cvar in c("c000001", "c000002", "c000003", "c000004", 
              "c000005", "c000006", "c000010", 
              "c0000015", "c000020", "c000200")) {
  plt <- ggplot(data = mafd_df, aes(x = .data[[cvar]])) + 
    geom_histogram() + 
    facet_wrap(~MAFD, ncol = 1) + 
    theme_bw()
  print(plt)
}

ggplot(data = mafd_df, aes(x = c000005)) + 
  geom_histogram() + 
  facet_wrap(~MAFD, ncol = 1) + 
  theme_bw()




############################
## Run automated detection model





















############# Old attempt at joining acoustic indices to bird data
# empty_index_names_df <- data.frame(
#   matrix(nrow = 1, ncol = length(names(acoustic_index_files))))
# colnames(empty_index_names_df) <- names(acoustic_index_files)
# a_dat <- bind_cols(a_dat, empty_index_names_df)
# rm(empty_index_names_df)

# this array will have the dimensions as follows:
# dim 1 (rows) = 30 second recording segments
# dim 2 (cols) = bird species, 
# dim 3 (slices) = acoustic indices, 
# dim 4 (??) = frequency bins
# bird_array <- array(NA, dim = c(nrow(a_dat), ncol(a_dat), 
#                                 length(acoustic_index_files), 256), 
#                   dimnames = list(a_dat$seg_id, colnames(a_dat)[bird_sp_cols], 
#                                   names(acoustic_index_files),
#                                   colnames(acoustic_index_files$ACI)[2:257]))
# 
# 
# # bind all acoustic index values into a single df
# acoustic_indices <- bind_rows(acoustic_index_files)
# rm(acoustic_index_files)

# add acoustic index values for all indices to each segment in hand-labelled
# bird identifications data frame


osc_data <- left_join(osc_data, bat_calls, by = c("filename" = "filename", 
                                                  "Index" = "minute"))


