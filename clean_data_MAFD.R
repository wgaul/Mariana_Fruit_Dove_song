#####################
## Load and clean Mariana Fruit Dove song data
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 11 July 2022
## last modified: 1 January 2023
######################

margo_dove <- read_csv("../data/Margo_Fruit_Dove_Sound_Data_12Aug2022.csv")
wg_ebird <- read_csv("../data/wg_ebird_data_01Jan2023.csv")

## ------------- Margo data -------------------
# drop blank rows
margo_dove <- margo_dove[which(!is.na(margo_dove$filename)), ]

# drop rows that are duplicates from other recorders
margo_dove <- margo_dove[-which(grepl(".*from Jaden.*", margo_dove$notes)), ]
# drop rows that have outdated data
margo_dove <- margo_dove[-which(grepl(".*should not be used.*", 
                                      margo_dove$notes)), ]
# drop rows that are placeholders for files with no measurable songs
margo_dove <- margo_dove[which(!is.na(margo_dove$`start_time_of_song_hh:mm:ss`)), ]

### read in metadata for recordings
# metadata for wg's audio recordings
wg_met <- read.csv("~/Documents/Data_Analysis/bird_song/data/METADATA_bird_audio.csv")
colnames(wg_met)[colnames(wg_met) == "file_name"] <- "filename"
colnames(wg_met)[colnames(wg_met) == "date_recorded_ddmmyyyy"] <- "date_ddmmyyyy"
colnames(wg_met)[colnames(wg_met) == "recording_start_time"] <- "local_time_hhmm"
wg_met$island <- wg_met$locality
wg_met$island <- gsub(".*Saipan.*", "Saipan", wg_met$island)
wg_met$island <- gsub(".*Tinian.*", "Tinian", wg_met$island)
wg_met$island <- gsub(".*Aguiguan.*", "Aguiguan", wg_met$island)
wg_met$island <- gsub(".*Rota.*", "Rota", wg_met$island)
wg_met$date_ddmmyyyy <- as.Date(wg_met$date_ddmmyyyy, 
                                format = "%d/%m/%Y")

# metadata for Ellie's Aguiguan ARU recordings
ag_met <- read.csv("~/Documents/Saipan_ecology/data/audio_recordings/aru_metadata_aguiguan_april2022.csv")
colnames(ag_met)[colnames(ag_met) == "file_name"] <- "filename"
colnames(ag_met)[colnames(ag_met) == "lat"] <- "latitude"
colnames(ag_met)[colnames(ag_met) == "long"] <- "longitude"
ag_met$island <- "Aguiguan"
ag_met$date_ddmmyyyy <- gsub("SWIFT.._", "", ag_met$filename)
ag_met$date_ddmmyyyy <- gsub("_.*", "", ag_met$date_ddmmyyyy)
ag_met$date_ddmmyyyy <- as.Date(as.character(ag_met$date_ddmmyyyy), 
                                format = "%Y%m%d")

# metadata for Macaulay Library recordings
ml_met <- read.csv("~/Documents/Data_Analysis/bird_song/Macaulay_audio/Ptilinopus_roseicapilla/METADATA.csv")
ml_met$date_ddmmyyyy <- as.Date(ml_met$date_ddmmyyyy, 
                                format = "%d/%m/%Y")

# metadata for Haldre's Forest Trajectories recordings
# As of 12 July 2022, the metadata for the Forest Trajectories recordings is
# not clear to me.  I see some UTM coordinates, but I cannot easily figure out
# how to match coordinates to locations in the file names of the audio 
# recordings.  
# For now, I will generate Island and location IDs from the filenames for the
# forest trajectories recordings.  I do not actually need geographic coordinates
# for any analyses I'm currently planning, as long as I can identify different
# locations.

# join all metadata
met <- bind_rows(wg_met, ag_met) %>%
  bind_rows(ml_met) 
# %>%
#   select(-gain_setting, -captivity, -distance_to_target_m, -species, 
#          -common_name, -notes)

met$filename <- gsub(".wav|.WAV|.mp3", "", met$filename)


#### clean data
margo_dove$filename <- gsub("Macaulay ", "", margo_dove$filename)
# make frequency numeric
margo_dove$Freq_peak_S1 <- gsub(" Hz.*", "", margo_dove$Freq_peak_S1)
margo_dove$Freq_peak_S1 <- gsub("Hz.*", "", margo_dove$Freq_peak_S1)
margo_dove$Freq_peak_S1 <- gsub("HZ.*", "", margo_dove$Freq_peak_S1)
margo_dove$Freq_peak_S1 <- as.numeric(margo_dove$Freq_peak_S1)

# make a song ID column
margo_dove$song_id <- paste(margo_dove$filename, 
                            margo_dove$`start_time_of_song_hh:mm:ss`, sep = "_")

# join recording metadata onto song measurements
margo_dove <- left_join(margo_dove, met)

# Fill in island value for Haldre's forest trajectories recordings
margo_dove$island[which(grepl(".*rota.*", margo_dove$filename))] <- "Rota"
margo_dove$island[which(grepl(".*Saipan.*", margo_dove$filename))] <- "Saipan"

# fill in location names for Haldre's forest trajectories recordings
margo_dove$location_name[grepl("rota_.*|Saipan_", margo_dove$filename)] <- 
  gsub("^...._|^......_", "", 
       margo_dove$filename[grepl("rota_.*|Saipan_", margo_dove$filename)])
margo_dove$location_name[grepl("rota_.*|Saipan_", margo_dove$filename)] <- 
  gsub("_.*$", "", 
       margo_dove$location_name[grepl("rota_.*|Saipan_", margo_dove$filename)])

# make unique location identifier
margo_dove$location_id <- paste(margo_dove$island, margo_dove$location_name, 
                                margo_dove$latitude, margo_dove$longitude, 
                                sep = "_")

# extract dates from Haldre Forest Trajectory filenames
h_dates <- gsub(".*_", "", margo_dove$filename[grepl("Saipan_|rota_", 
                                                     margo_dove$filename)])
h_dates <- as.Date(h_dates, tryFormats = c("%d%b%y", "%d%b%Y"))
margo_dove$date_ddmmyyyy[grepl("Saipan_|rota_", margo_dove$filename)] <- h_dates
rm(h_dates)

margo_dove$year <- year(margo_dove$date_ddmmyyyy)
margo_dove$month <- month(margo_dove$date_ddmmyyyy)
margo_dove$`start_time_of_song_hh:mm:ss` <- as.POSIXct(
  margo_dove$`start_time_of_song_hh:mm:ss`, 
  format = "%H:%M:%S")

colnames(margo_dove) <- gsub(":", "", colnames(margo_dove))

## ---------------- end Margo Data ----------------------------


## ---------------- wg eBird data --------------------------
# subset to Mariana Islands (CNMI and Guam)
wg_ebird <- wg_ebird[wg_ebird$Latitude < 20 & wg_ebird$Longitude > 120, ]

# find locations at which a MAFD was ever observed
locs_mafd <- unique(wg_ebird$`Location ID`[wg_ebird$`Scientific Name` == "Ptilinopus roseicapilla"])

# subset to only MAFD locations
wg_ebird <- wg_ebird[wg_ebird$`Location ID` %in% locs_mafd, ]

wg_ebird_wide <- wg_ebird[, which(colnames(wg_ebird) %in% 
                            c("Submission ID", "Scientific Name", "Count", 
                              "State/Province", "Location ID", 
                              "Location", "Latitude", "Longitude", 
                              "Date", "Time", "Protocol", "Duration (Min)", 
                              "All Obs Reported", "Distance Traveled (km)", 
                              "Number of Observers"))]
wg_ebird_wide <- pivot_wider(wg_ebird_wide, names_from = "Scientific Name",
                             values_from = "Count", 
                             values_fill = 0)

wg_ebird_wide <- wg_ebird_wide[order(wg_ebird_wide$`Submission ID`), ]

# drop incomplete checklists
wg_ebird_wide <- wg_ebird_wide[wg_ebird_wide$`All Obs Reported` == 1, ]
# Fill distance traveled with 0 if NA
wg_ebird_wide$`Distance Traveled (km)`[which(
  is.na(wg_ebird_wide$`Distance Traveled (km)`))] <- 0

# Make a column indicating whether MAFD was singing on the checklist
wg_ebird_wide$breeding_code_MAFD <- NA
for(i in 1:nrow(wg_ebird_wide)) {
  dat <- wg_ebird[wg_ebird$`Submission ID` == wg_ebird_wide$`Submission ID`[i] &
                    wg_ebird$`Scientific Name` == "Ptilinopus roseicapilla", ]
  if(nrow(dat) > 1) stop("I think dat should only ever be 1 row.  It is a df of this species on a single checklist.")
  # fill Breeding code if this checklist had a MAFD on it
  if(nrow(dat) > 0) wg_ebird_wide$breeding_code_MAFD[i] <- dat$`Breeding Code`
}

# add day_of_year column
wg_ebird_wide$day_of_year <- yday(wg_ebird_wide$Date)
wg_ebird_wide$month <- month(wg_ebird_wide$Date)

colnames(wg_ebird_wide) <- gsub(" ", "_", colnames(wg_ebird_wide))
colnames(wg_ebird_wide) <- gsub("\\(|\\)", "", colnames(wg_ebird_wide))

# make Location_ID a factor as required by mgcv
wg_ebird_wide$Location_ID <- factor(as.character(wg_ebird_wide$Location_ID))
# make time number of seconds after midnight
wg_ebird_wide$tod_sec <- as.numeric(wg_ebird_wide$Time)

## ------------- end wg eBird data -------------------------