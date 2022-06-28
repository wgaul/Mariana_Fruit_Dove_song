#####################
## Explore Mariana Fruit Dove song parameters
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 11 March 2022
## last modified: 24 June 2022
######################

library(tidyverse)
library(GGally)

setwd("~/Documents/Data_Analysis/bird_song/")

# dove_song <- read_csv("./data/Jaden_Fruit_Dove_Sound_Data.csv",
                      # na = c("NA", "N/A", ""))
# dove_song[which(dove_song$Island == "rota"), 1:2]
# dove_song
# margo_dove <- read_csv("./data/Margo_v_WG_Fruit_Dove_Sound_Data_13June2022.csv")
margo_dove <- read_csv("./data/Margo_Fruit_Dove_Sound_Data_24June2022.csv")

# drop blank rows
margo_dove <- margo_dove[which(!is.na(margo_dove$Filename)), ]

# drop rows that are duplicates from other recorders
margo_dove <- margo_dove[-c(1:2), ]

# Make island column
# This will require incorporating info from another file at some point
margo_dove$Island <- gsub("_.*", "", margo_dove$Filename)
warning("Update this to incorporate info from another file at some point")
margo_dove$Island[which(margo_dove$Filename %in% 
                          c("0226_084100_MarianaFruitDove", 
                            "0306_071304_MarianaFruitDove", 
                            "MAFD_0620_065345", "MAFD_0620_072839", 
                            "Mariana_Fruit_Dove_20220416", 
                            "Mariana_Fruit_Dove_20220417"))] <- "Saipan"

# make frequency numeric
# dove_song$Freq_peak_S1 <- gsub(" Hz.*", "", dove_song$Freq_peak_S1)
# dove_song$Freq_peak_S1 <- as.numeric(dove_song$Freq_peak_S1)
margo_dove$Freq_peak_S1 <- gsub(" Hz.*", "", margo_dove$Freq_peak_S1)
margo_dove$Freq_peak_S1 <- gsub("Hz.*", "", margo_dove$Freq_peak_S1)
margo_dove$Freq_peak_S1 <- gsub("HZ.*", "", margo_dove$Freq_peak_S1)
margo_dove$Freq_peak_S1 <- as.numeric(margo_dove$Freq_peak_S1)

# make a song ID column
margo_dove$id <- paste(margo_dove$Filename, 
                       margo_dove$`start_time_of_song_hh:mm:ss`, sep = "_")

### Graphs
t_size <- 25

## Compare WG and Margo's measurements
margo_wg_long <- pivot_longer(margo_wg_dove, song_duration_sec:Freq_peak_S1)
# spread to get different columns for each observer
margo_wg_long$name <- paste(margo_wg_long$technician, margo_long$name, sep = "_")
# drop technician column so spreading works better
margo_wg_long <- margo_wg_long[, -which(colnames(margo_wg_long) == "technician")]
margo_wg_wide <- pivot_wider(margo_wg_long, id_cols = "id")

# get only songs measured by all observers
wg_songs <- unique(margo_wg_dove$id[margo_wg_dove$technician == "WG"])
mq_songs <- unique(margo_wg_dove$id[margo_wg_dove$technician == "MQ"])
shared_songs <- mq_songs[which(mq_songs %in% wg_songs)]

# put songs measured by all observers into a df
shared_song_df <- margo_wg_wide[which(margo_wg_wide$id %in% shared_songs), ]

plot(shared_song_df$MQ_song_duration_sec, shared_song_df$WG_song_duration_sec)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_song_duration_sec, shared_song_df$WG_song_duration_sec, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_S1_to_S2, shared_song_df$WG_S1_to_S2)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_S1_to_S2, shared_song_df$WG_S1_to_S2, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_S2_to_S3, shared_song_df$WG_S2_to_S3)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_S2_to_S3, shared_song_df$WG_S2_to_S3,
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_S3_to_S4, shared_song_df$WG_S3_to_S4)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_S3_to_S4, shared_song_df$WG_S3_to_S4, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_S4_to_S5, shared_song_df$WG_S4_to_S5)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_S4_to_S5, shared_song_df$WG_S4_to_S5, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_DS1, shared_song_df$WG_DS1)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_DS1, shared_song_df$WG_DS1, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_DS2, shared_song_df$WG_DS2)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_DS2, shared_song_df$WG_DS2, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_DS3, shared_song_df$WG_DS3)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_DS3, shared_song_df$WG_DS3, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_DS4, shared_song_df$WG_DS4)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_DS4, shared_song_df$WG_DS4, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_DS5, shared_song_df$WG_DS5)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_DS5, shared_song_df$WG_DS5, 
    use = "complete.obs", method = c("spearman"))
plot(shared_song_df$MQ_Freq_peak_S1, shared_song_df$WG_Freq_peak_S1)
abline(a = 0, b = 1)
cor(shared_song_df$MQ_Freq_peak_S1, shared_song_df$WG_Freq_peak_S1, 
    use = "complete.obs", method = c("spearman"))

margo_dove <- margo_dove[margo_dove$Island != "old", ]

song_length_plot <- ggplot(data = margo_dove) + 
  geom_histogram(aes(x = song_duration_sec), binwidth = 0.4) + 
  facet_wrap(~factor(Island, 
                     levels = c("rota", "Saipan", "SWIFT03"), 
                     labels = c("Rota", "Saipan", "Aguiguan")), ncol = 1) + 
  xlim(0, 15) + 
  xlab("Song duration\n(seconds)") + 
  ylab("Number of Songs") + 
  theme_bw()
song_length_plot

freq_plot <- ggplot(data = margo_dove) + 
  geom_histogram(aes(x = Freq_peak_S1), binwidth = 12) + 
  facet_wrap(~factor(Island, 
                     levels = c("rota", "Saipan", "SWIFT03"), 
                     labels = c("Rota", "Saipan", "Aguiguan")), ncol = 1) + 
  ylab("Number of songs") + 
  xlab("Frequency of first syllable (Hz)") + 
  xlim(0, NA) + 
  theme_bw()
freq_plot

ggplot(data = margo_dove) +
  geom_boxplot(aes(x = Island, y = Freq_peak_S1))








## As of 19 April 2022:
## The comparison between Margo and Jaden is difficult because we do not have
## a unique song ID.  Will have to figure this out sometime, but it is low
## priority.
# # compare Margo's and Jaden's measurements
# identical(colnames(margo_dove), colnames(dove_song))
# # make a unique identifier for each song
# dove_song$song_id <- paste(dove_song$Filename, dove_song$song_duration_sec,
#                            sep = "_")
# margo_dove$song_id <- paste(margo_dove$Filename, margo_dove$song_duration_sec,
#                            sep = "_")
# jaden_margo_match_subset <- dove_song[which(dove_song$song_id %in%
#                                               margo_dove$song_id), ]
#
# for(i in 3:13) {
#   plot(dove_song[, i][[1]], margo_dove[, i][[1]])
# }

jaden_margo_match_subest <- dove_song[which(dove_song$Filename %in% 
                                              margo_dove$Filename), ]

mq <- margo_dove[-c(1, 6, 7), ]
jv <- jaden_margo_match_subest[c(7, 2, 13, 14, 1, 16), ]

for(i in 3:13) {
  plot(mq[, i][[1]] ~ jv[, i][[1]], ylab = colnames(mq)[i])
  abline(a = 0, b = 1)
}




# Pairs plot of all parameters
ggpairs(
  data = dove_song, 
  axisLabels = "none", 
  columns = 3:17)

# look at syllable length vs. gap length
ggpairs(
  data = dove_song, 
  axisLabels = "none", 
  columns = 8:17)

# start to start vs. gap length
ggpairs(
  data = dove_song, 
  axisLabels = "none", 
  columns = c(4:7, 13:17))

# start to start vs. syllable length
ggpairs(
  data = dove_song, 
  axisLabels = "none", 
  columns = c(4:7, 8:12))

# graph song length
song_length_plot <- ggplot(
  data = dove_song, 
  aes(x = factor(Island, levels = c("rota", "Saipan"), 
                 labels = c("Rota", "Saipan")), y = song_duration_sec)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) +
  ylim(c(0, max(dove_song$song_duration_sec))) + 
  ylab("Song duration\n(seconds)") + 
  xlab("") + 
  theme_bw() + 
  theme(text = element_text(size = t_size))
song_length_plot

# graph frequency peak
freq_plot <- ggplot(
  data = dove_song, 
  aes(x = factor(Island, levels = c("rota", "Saipan"), 
                 labels = c("Rota", "Saipan")), y = Freq_peak_S1)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  ylim(c(0, max(dove_song$Freq_peak_S1))) +
  theme_bw() + 
  ylab("Frequency (Hz)") + 
  xlab("") + 
  theme(text = element_text(size = t_size))
freq_plot

# graph time between first notes
ggplot(data = dove_song, aes(x = factor(Island), y = S1_to_S2)) + 
  geom_boxplot() + 
  geom_point() +
  ylim(c(0, max(dove_song$S1_to_S2))) + 
  theme_bw()

# print number of songs from each island
table(dove_song$Island)

# save plots
ggsave("song_length.jpg", song_length_plot + 
         theme(text = element_text(size = t_size)), 
       width = 14, height = 14, units = "cm", device = "jpg")
ggsave("Freq_S1.jpg", freq_plot + 
         theme(text = element_text(size = t_size)), 
       width = 14, height = 14, units = "cm", device = "jpg")
