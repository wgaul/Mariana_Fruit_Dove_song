#####################
## Analyse Mariana Fruit Dove song parameters
## 
## This script organizes the workflow for analysing Mariana Fruit Dove songs
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 11 July 2022
## last modified: 11 July 2022
######################

library(tidyverse)
library(GGally)
library(lubridate)
library(lme4)

t_size <- 25 # text size for graphs

setwd("~/Documents/Data_Analysis/bird_song/Mariana_Fruit_Dove_song/")

# load data and clean data
source("clean_data_MAFD.R")

# fit mixed effects models to estimate effect of island on song parameters
source("fit_mixed_effects.R")


### exploratory graphs
# how many locations per island?
for(isl in unique(margo_dove$island)) {
  print(c(isl, " n locations: ", 
          length(unique(margo_dove$location_id[margo_dove$island == isl & 
                                                 !is.na(margo_dove$island)]))))
  
}

# how many songs per island?
for(isl in unique(margo_dove$island)) {
  print(c(isl, " n songs: ", 
          length(unique(margo_dove$song_id[margo_dove$island == isl & 
                                             !is.na(margo_dove$island)]))))
  
}

hist(table(margo_dove$location_id), breaks = 100, xlab = "N Songs", 
     ylab = "N locations")

ggplot(data = margo_dove[grepl("SWIFT.*", margo_dove$filename), ], 
       aes(x = start_time_of_song_hhmmss, y = Freq_peak_S1))+
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~ filename)


