#!/bin/bash
## bash script for running the Towsey sound analysis software
## Towsey software is installed in ~/.local/share/AP on my computer
## This script should be run from within the directory that contains the .WAV files
## created: 10 Dec 2021, modified 21 June 2022
## author: Willson Gaul willson.gaul@gmail.com

if [ ! -d "IndicesOutput/" ]; then
  mkdir IndicesOutput/
fi

# Run AP to analyze audio recordings
# Run for upper case file endings
for name in $(ls *.WAV)
do
## printf $name
~/.local/share/AP/AnalysisPrograms audio2csv $name Towsey.Acoustic.MAFD30Sec.yml IndicesOutput/
done

# Run for lower case file endings
for name in $(ls *.wav)
do
## printf $name
~/.local/share/AP/AnalysisPrograms audio2csv $name "Towsey.Acoustic.MAFD30Sec.yml" "IndicesOutput/"
done

# copy spectrograms for indices of interest to a separate directory
# for ease of viewing
if [ ! -d "IndicesOutput/OSC/" ]; then
  mkdir IndicesOutput/OSC/
fi

cp ./IndicesOutput/Towsey.Acoustic/*OSC.png ./IndicesOutput/OSC/
