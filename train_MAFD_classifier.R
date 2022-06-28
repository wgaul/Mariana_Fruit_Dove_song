#####################
## Train automated identification model for Mariana Fruit Dove songs
## in sound recordings
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 24 June 2022
## last modified: 28 June 2022
######################

library(wgutil)
library(Hmisc)
library(tidyverse)

setwd("~/Documents/Data_Analysis/bird_song/Mariana_Fruit_Dove_song/")

# read in sound species annotations from Alexander
a_dat <- data.frame(read_csv("../data/ARU_recording_annotation_Alex.csv", 
                             na = c("NA", "N/A", "")))

# drop an extra column from Alex's data
a_dat <- a_dat[, -which(colnames(a_dat) == "...26")]

# tidy column names
colnames(a_dat) <- gsub(":|-|\\.", "_", colnames(a_dat))

# make sound segment identifier
a_dat$seg_id <- paste(a_dat$filename, a_dat$sec_interval_hh_mm_ss, sep = "_")

# make observer ID
a_dat$observer <- "AT"

# drop Alex's duplicate Kalabera data
a_dat <- a_dat[-c(1:29), ]

# convert "x" marks to TRUE
a_dat[, 3:20] <- !is.na(a_dat[, 3:20]) 

# which columns are bird species detection columns?
bird_sp_cols <- c(2:18, 20)

#################################
## Join acoustic index values to Alex's annotated minutes
# set location of Towsey index data
data_location <- "~/Documents/Saipan_ecology/haldre_forest_trajectories/forest trajectories/data/raw/SoundRecorder/IndicesOutput/Towsey.Acoustic/"
acoustic_index_files <- list(ACI = c(), BGN = c(), CVR = c(), DIF = c(), 
                             ENT = c(), EVN = c(), OSC = c(), PMN = c(), 
                             RHZ = c(), RNG = c(), RPS = c(), RVT = c(), 
                             SPT = c(), SUM = c())


for(i in 1:length(acoustic_index_files)) {
  # get file names for all acoustic index .csv files
  ind_name <- names(acoustic_index_files)[i]
  acoustic_index_files[[i]] <- list.files(data_location)
  acoustic_index_files[[i]] <- acoustic_index_files[[i]][grepl(
    paste0(".*", ind_name, ".csv"), 
    acoustic_index_files[[i]])]
  
  # read in all data for this index
  acoustic_index_files[[i]] <- lapply(
    acoustic_index_files[[i]], 
    FUN = function(x, data_location, acoustic_index) {
      dt <- read.csv(paste0(data_location, x), stringsAsFactors = FALSE)
      dt$acoustic_index <- acoustic_index
      # add the recording filename as a column
      dt$filename <- gsub("__Towsey.*", "", x) 
      dt
    }, data_location = data_location, acoustic_index = ind_name)
  
  # bind data from all recordings into a single df
  acoustic_index_files[[i]] <- bind_rows(acoustic_index_files[[i]])
  
  # TODO: Extract date and time from filename and store in two separate columns
}

empty_index_names_df <- data.frame(
  matrix(nrow = 1, ncol = length(names(acoustic_index_files))))
colnames(empty_index_names_df) <- names(acoustic_index_files)
a_dat <- bind_cols(a_dat, empty_index_names_df)
rm(empty_index_names_df)

# this array will have the dimensions as follows:
# dim 1 (rows) = 30 second recording segments
# dim 2 (cols) = bird species, 
# dim 3 (slices) = acoustic indices, 
# dim 4 (??) = frequency bins
bird_array <- array(NA, dim = c(nrow(a_dat), ncol(a_dat), 
                                length(acoustic_index_files), 256), 
                  dimnames = list(a_dat$seg_id, colnames(a_dat)[bird_sp_cols], 
                                  names(acoustic_index_files),
                                  colnames(acoustic_index_files$ACI)[2:257]))


# bind all acoustic index values into a single df
acoustic_indices <- bind_rows(acoustic_index_files)
rm(acoustic_index_files)

# add acoustic index values for all indices to each segment in hand-labelled
# bird identifications data frame


osc_data <- left_join(osc_data, bat_calls, by = c("filename" = "filename", 
                                                  "Index" = "minute"))

stop("Here 24 June")



############################
## Run automated detection model