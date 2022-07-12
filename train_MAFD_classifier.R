#####################
## Train automated identification model for Mariana Fruit Dove songs
## in sound recordings
## 
## TODO: Add acoustic indices for: 
##   - Ellie Saipan random minute ARU recordings
##   - targeted hand-held MAFD recordings (e.g. my recordings posted to eBird)
##   - Consider taking equal numbers of detections and non-detections from each
##      file for traning data, to avoid the model learning background noise 
##      associated with time of day (e.g. from early morning recordings where 
##      there is a single song during 2 hours of recording)
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 24 June 2022
## last modified: 30 June 2022
######################

library(Hmisc)
library(pROC)
# library(psych)
library(wgutil)
library(randomForest)
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

a_dat_pos <- a_dat[which(a_dat$MAFD == TRUE), ]
a_dat_neg <- a_dat[which(a_dat$MAFD == FALSE), ]

# get equal numbers of dets and nondets from alex's recordings
a_dat <- slice_sample(a_dat_neg, n = nrow(a_dat_pos)) %>%
  bind_rows(a_dat_pos)
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

#################################
## Match acoustic index values to annotated minutes
# set location of Towsey index data
# TODO: Add acoustic indices for: 
#   - Ellie Saipan random minute ARU recordings
#   - targeted hand-held MAFD recordings (e.g. my recordings posted to eBird)
forest_traj_data_location <- "~/Documents/Saipan_ecology/haldre_forest_trajectories/forest trajectories/data/raw/SoundRecorder/IndicesOutput/Towsey.Acoustic/"
aguiguan_aru_data_location <- "~/Documents/Saipan_ecology/data/audio_recordings/IndicesOutput/Towsey.Acoustic/"
acoustic_index_files <- list(ACI = c(), BGN = c(), CVR = c(), DIF = c(), 
                             ENT = c(), EVN = c(), OSC = c(), PMN = c(), 
                             RHZ = c(), RNG = c(), RPS = c(), RVT = c(), 
                             SPT = c(), SUM = c())


for(i in 1:length(acoustic_index_files)) {
  # get file names for all acoustic index .csv files
  ind_name <- names(acoustic_index_files)[i]
  # ...for forest trajectory recordings
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
  colnames(acoustic_index_files[[i]])[which(
    grepl(".*c00.*", colnames(acoustic_index_files[[i]])))] <- paste0(
      ind_name, colnames(acoustic_index_files[[i]])[which(
        grepl(".*c00.*", colnames(acoustic_index_files[[i]])))]
    )
}



### make a df for Mariana Fruit Dove data
# sample non-detection minutes from the recordings Margo analyzed
mq_non_det_sample <- bind_rows(lapply(acoustic_index_files, FUN = function(x) {
  x[, which(colnames(x) %in% c("filename", "seg_id"))]
}))
mq_non_det_sample <- unique(mq_non_det_sample)
# keep only data from files that are in Margo's data, so that we do not get 
# training data from times of the day when there are no MAFDs singing
mq_non_det_sample <- mq_non_det_sample[mq_non_det_sample$filename %in% 
                                         mq$filename , ]

# select only segments that are not in Margo's data
mq_non_det_sample <- mq_non_det_sample[mq_non_det_sample %nin% mq$seg_id]
# select only segments that are not in Alex's data (as either dets or non-dets)
mq_non_det_sample <- mq_non_det_sample[mq_non_det_sample %nin% a_dat$seg_id]
# select as many non-detection segments as we have detection segments
mq_non_det_sample <- slice_sample(mq_non_det_sample, n = nrow(mq), 
                                  replace = F)
mq_non_det_sample <- bind_rows(lapply(
  acoustic_index_files, FUN = function(x, non_det_seg_ids) {
    x[x$seg_id %in% non_det_seg_ids, ]
  }, non_det_seg_ids = mq_non_det_sample$seg_id)) %>%
  select(filename, seg_id) %>%
  slice_sample(n = sum(mq$MAFD)) %>%
  mutate(MAFD = FALSE)

# select the columns we want from Alex's data
mafd_df <- a_dat[, which(colnames(a_dat) %in% 
                           c("filename", "seg_id", 
                             "MAFD"))]
# Add Margo's data
mafd_df <- bind_rows(mafd_df, mq[, which(colnames(mq) %in% 
                                           c("filename", "seg_id", 
                                             "MAFD"))])
# add non-detections from Margo's recordings
mafd_df <- bind_rows(mafd_df, mq_non_det_sample)

table(mafd_df$filename)
summary(as.numeric(table(mafd_df$seg_id)))
table(mafd_df$MAFD)
table(mafd_df$MAFD, mafd_df$filename) # we want at least some T and F from each file
# TODO: Consider taking equal numbers of detections and non-detections from each
# file for traning data, to avoid the model learning background noise associated
# with time of day (e.g. from early morning recordings where there is a single
# song during 2 hours of recording)


###### look at some preliminary correlations to choose predictor vars.
## each column is an index value for a frequency band.
## 256 freq bands for 22000 Hz (see Towsey.Acoustic.MAFD30Sec.yml)
## 22000/256 = 85.94 Hz per frequency band
## MAFD is between 300 and 600 Hz. 
## 300/85.94 = 3rd band (named band 2 b/c numbering starts at 0)
## 400/85.94 = band 3
## 500/85.94 = band 4
## 600/85.94 = band 5
## 700/85.94 = band 7
## 1000/85.94 = band 10
## TODO: Make sure those columns are the frequency bands we want
warning("We MUST make sure we know what the frequencies of each column are.")

pred_cols <- c("c000002", "c000003", "c000004", "c000005", "c000006", "c000010", 
               "c000015", "c000020", "c000200")
pred_cols <- c(pred_cols, "seg_id")

for(i in 1:length(acoustic_index_files)) {
  preds <- paste0(acoustic_index_files[[i]]$acoustic_index[1], pred_cols)
  mafd_df <- left_join(
    mafd_df, acoustic_index_files[[i]][, which(
      colnames(acoustic_index_files[[i]]) %in% c("filename", "seg_id", preds))])
}

# which column have predictor variables right now? (for quick exploration)
names(mafd_df)
pdf("explore_MAFD_predictor_indices.pdf")
for(cvar in colnames(mafd_df)[4:ncol(mafd_df)]) {
  plt <- ggplot(data = mafd_df, aes(x = .data[[cvar]])) + 
    geom_histogram(bins = 100) + 
    facet_wrap(~MAFD, ncol = 1) + 
    theme_bw()
  print(plt)
  
  plt <- ggplot(data = mafd_df, aes(x = MAFD, y = .data[[cvar]])) + 
    geom_boxplot() + 
    geom_point() + 
    theme_bw()
  print(plt)
}
dev.off()

# look at particular predictors by file name
ggplot(data = mafd_df, aes(x = droplevels(factor(filename)), y = SUMc000002, 
                           color = MAFD)) + 
  geom_boxplot() + 
  # geom_point() +
  theme(axis.text.x = element_text(angle = -90))

############################
## Run automated detection model
## Fit a random forest model

# specify which predictor variables to use based on a-priori expectations b/c
# of frequency of songs, and preliminary visual exploration of correlation 
# between song presence and acoustic index values
# chosen_preds <- c("ACIc000002", 
#                   "BGNc000002", "BGNc000004", "BGNc000006", "BGNc0000010", 
#                   "BGNc000200", 
#                   "CVRc000010", 
#                   "DIFc000002", "DIFc000004", "DIFc000006", "DIFc0000010", 
#                   "DIFc000015", 
#                   "ENTc000002", "ENTc000003", 
#                   "EVNc000010", 
#                   "OSCc000005", "OSCc000015", 
#                   "RPSc000005", "OSCc000006", "OSCc0000010", 
#                   "SUMc000002", "SUMc000003", "SUMc000004")

chosen_preds <- c("EVNc000004", "EVNc000005", "EVNc000010", 
                  "OSCc000015", 
                  "SPTc000002", "SPTc000004")

mafd_df <- mafd_df[, which(colnames(mafd_df) %in% c("filename", "seg_id", 
                                                    "MAFD", chosen_preds))]

# specify CV folds
mafd_df$fold <- sample(1:3, size = nrow(mafd_df), replace = T)

# drop rows for which we do not have predictor data
mafd_df <- mafd_df[complete.cases(mafd_df), ]

## fit model
mafd_df$pred_test_fold <- NA
mafd_df$pred_train_fold <- NA
mafd_mods <- list()
for(tst_fold in unique(mafd_df$fold)) {
  # train model using training data folds
  train_dat <- mafd_df[mafd_df$fold != tst_fold, ]
  mod_rf <- randomForest(x = train_dat[, grepl("...c0.*", colnames(train_dat))], 
                         y = factor(as.character(train_dat$MAFD), 
                                    levels = c("TRUE", "FALSE"), 
                                    labels = c("TRUE", "FALSE")), 
                         ntree = 2000, replace = T, importance = T, 
                         nodesize = 1)
  # get predictions to test CV fold
  mafd_df$pred_test_fold[mafd_df$fold == tst_fold] <- predict(
    mod_rf, 
    newdata = mafd_df[mafd_df$fold == tst_fold, 
                      which(colnames(mafd_df) %nin% 
                              c("pred_test_fold", "pred_train_fold"))], 
    type = "prob")[, "TRUE"]
  mafd_df$pred_train_fold[mafd_df$fold != tst_fold] <- predict(
    mod_rf, 
    newdata = mafd_df[mafd_df$fold != tst_fold, 
                      which(colnames(mafd_df) %nin% 
                              c("pred_test_fold", "pred_train_fold"))], 
    type = "prob")[, "TRUE"]
  
  # save fitted model in list
  mafd_mods[[tst_fold]] <- mod_rf
}
mafd_mods

## evaluate model performance (this is CV performance)
# AUC on training folds
roc(
  response = factor(as.character(mafd_df$MAFD), 
                    labels = c("1", "0"), 
                    levels = c("TRUE", "FALSE")), 
  predictor = mafd_df$pred_train_fold, 
  levels = c("0", "1"), direction = "<", plot = T)

# AUC on test folds
roc(
  response = factor(as.character(mafd_df$MAFD), 
                    labels = c("1", "0"), 
                    levels = c("TRUE", "FALSE")), 
  predictor = mafd_df$pred_test_fold, 
  levels = c("0", "1"), direction = "<", plot = T)

# Kappa
thresh <- as.numeric(seq(from = 0, to = 1, by = 0.05))
names(thresh) <- thresh
kappas <- sapply(thresh, FUN = function(thresh, dat) {
  vals <- data.frame(resp = factor(as.character(dat$MAFD), 
                                   levels = c("TRUE", "FALSE"), 
                                   labels = c("TRUE", "FALSE")), 
                     pred = factor(as.numeric(dat$pred_test_fold) > thresh))
  psych::cohen.kappa(vals)$kappa
  psych::cohen.kappa(vals)$kappa
}, dat = mafd_df)
kappas
plot(kappas ~ as.numeric(names(kappas)))

# max Cohen's Kappa (CV)
max(kappas)

# set threshold for prediction 
# Can use maximised kappa, or adjust to prioritize sensitivity or specificity
thresh <- as.numeric(names(kappas)[which(kappas == max(kappas))])[1]

# sensitivity at threshold that maximised Kappa
sensitivity(threshold = thresh, 
            responses = mafd_df$MAFD, 
            predictions = as.numeric(mafd_df$pred_test_fold))
# specificity at threshold that maximised Kappa
specificity(threshold = thresh, 
            responses = mafd_df$MAFD, 
            predictions = as.numeric(mafd_df$pred_test_fold))


## look at variable importance (just for the last model that fitted)
# TODO: loop this to look at all CV models
var_imp <- data.frame(mod_rf$importance)
var_imp$var <- rownames(var_imp)
var_imp <- var_imp[order(var_imp$MeanDecreaseGini, decreasing = T), ]
var_imp$var <- factor(as.character(var_imp$var), levels = var_imp$var, 
                      labels = var_imp$var,)
ggplot(data = var_imp, 
       aes(x = MeanDecreaseGini, y = var)) + 
  geom_col()

# get partial dependence for each variable
pl <- list()
for(vb in 1:length(var_imp$var)) {
  pd <- data.frame(partialPlot(mod_rf, 
                               pred.data = data.frame(mafd_df), 
                               x.var = as.character(var_imp$var)[vb], 
                               which.class = "TRUE", plot = FALSE))
  pd$variable <- var_imp$var[vb]
  pl[[vb]] <- pd
}
pl <- bind_rows(pl)

ggplot(data = pl, aes(x = x, y = y)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~factor(variable)) + 
  ggtitle("partial dependence") + 
  theme_bw()

