#####################
## Analyse Mariana Fruit Dove song parameters
## 
## This script organizes the workflow for analysing Mariana Fruit Dove songs
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 11 July 2022
## last modified: 2 Aug 2022
######################

library(tidyverse)
library(GGally)
library(lubridate)
library(lme4)

t_size <- 25 # text size for graphs

setwd("~/Documents/Data_Analysis/bird_song/Mariana_Fruit_Dove_song/")

# load data and clean data
source("clean_data_MAFD.R")

# model annual detectability (i.e. singing)
source("annual_detectability_MAFD.R")

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
  # geom_smooth() + 
  facet_wrap(~ filename, ncol = 5)

# Freq by month
ggplot(data = margo_dove, aes(x = as.numeric(as.character(month)), 
                              y = Freq_peak_S1, colour = island)) + 
  geom_point() + 
  geom_smooth()

table(margo_dove$month)

# how many songs had countable B syllables?
length(unique(which(!is.na(margo_dove$N_B_syllables))))

# number of B syllables by month
ggplot(data = margo_dove, aes(x = as.numeric(as.character(month)), 
                              y = N_B_syllables, colour = island)) + 
  # geom_point() + 
  geom_jitter() #+
  # geom_smooth()

# number of B syllables by location
ggplot(data = margo_dove, aes(x = location_id, y = N_B_syllables, 
                              color = island)) + 
  geom_jitter() +
  ylim(0, 7)

warning("Put this in a better place later")
margo_dove$N_C_syllables <- margo_dove$N_C1_syllables + margo_dove$N_C2_syllables

# number of B syllables by month
ggplot(data = margo_dove, aes(x = as.numeric(as.character(month)), 
                              y = N_C_syllables, colour = island)) + 
  # geom_point() + 
  geom_jitter() #+
# geom_smooth()

# number of B syllables by C syllables
ggplot(data = margo_dove, aes(x = N_C_syllables, 
                              y = N_B_syllables)) + 
  geom_jitter(width = 0.1, height = 0.1)

# number of B syllables by first gap length
ggplot(data = margo_dove, aes(x = N_B_syllables, 
                              y = as.numeric(S1_to_S2))) + 
  # geom_point() + 
  geom_jitter() + 
  geom_smooth()

length(which(!is.na(margo_dove$N_B_syllables)))
hist(margo_dove$N_B_syllables)
table(margo_dove$N_B_syllables)
hist(margo_dove$N_C_syllables, breaks = 30)


ggplot(data = margo_dove, aes(x = N_C1_syllables, 
                              y = N_C2_syllables)) + 
  geom_point() + 
  # geom_jitter() + 
  geom_smooth()


ggplot(data = margo_dove, aes(x = N_C1_syllables, 
                              y = N_C2_syllables, 
                              color = song_duration_sec)) + 
  geom_point() + 
  geom_smooth() + 
  geom_jitter()




### compare Margo and Jaden's data
# jaden_dove <- read_csv("../data/Jaden_Fruit_Dove_Sound_Data_26Sep2022.csv")
# jaden_dove$Freq_peak_S1 <- gsub(".Hz", "", jaden_dove$Freq_peak_S1)
# jaden_dove$Freq_peak_S1 <- as.numeric(as.character(jaden_dove$Freq_peak_S1))
# md <- margo_dove[which(margo_dove$filename %in% unique(jaden_dove$filename)), ]
# md <- group_by(md, filename) %>%
#   arrange(filename, start_time_of_song_hhmmss, .by_group = TRUE)
# jaden_dove <- group_by(jaden_dove, filename) %>%
#   arrange(filename, start_time_of_song_hhmmss, .by_group = TRUE)
# 
# plot(md$song_duration_sec, jaden_dove$song_duration_sec)
# data.frame(md[which(jaden_dove$song_duration_sec - md$song_duration_sec > 0.5), ])
# data.frame(jaden_dove[which(jaden_dove$song_duration_sec - md$song_duration_sec > 0.5), ])
# plot(md$S1_to_S2, jaden_dove$S1_to_S2)
# plot(md$S2_to_S3, jaden_dove$S2_to_S3)
# plot(md$S3_to_S4, jaden_dove$S3_to_S4)
# plot(md$S4_to_S5, jaden_dove$S4_to_S5)
# plot(md$DS1, jaden_dove$DS1)
# plot(md$DS2, jaden_dove$DS2)
# plot(md$DS3, jaden_dove$DS3)
# plot(md$DS4, jaden_dove$DS4)
# plot(md$DS5, jaden_dove$DS5)
# plot(md$Freq_peak_S1, jaden_dove$Freq_peak_S1) ## look at these
# abline(coef = c(0, 1))
# data.frame(md[which(jaden_dove$Freq_peak_S1 - md$Freq_peak_S1 > 20), ])
# data.frame(jaden_dove[which(jaden_dove$Freq_peak_S1 - md$Freq_peak_S1 > 20), ])
# 
# plot(md$N_A_syllables, jaden_dove$N_A_syllables)
# table(md$N_A_syllables, jaden_dove$N_A_syllables)
# plot(md$N_B_syllables, jaden_dove$N_B_syllables)
# table(md$N_B_syllables, jaden_dove$N_B_syllables)  ## look at this
# md$N_C_syllables <- md$N_C1_syllables + md$N_C2_syllables
# plot(md$N_C_syllables, jaden_dove$N_C_syllables)
# abline(coef = c(0, 1))
# table(md$N_C_syllables, jaden_dove$N_C_syllables)
# 
# length(which(is.na(md$song_duration_sec)))
# length(which(is.na(jaden_dove$song_duration_sec)))
# View(md)
# View(jaden_dove)
# 
# length(which(is.na(md$N_B_syllables)))
# length(which(is.na(jaden_dove$N_B_syllables)))
# 
# length(which(is.na(md$N_C_syllables)))
# length(which(is.na(jaden_dove$N_C_syllables)))
