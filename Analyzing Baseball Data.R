install.packages("Lahman")
library(Lahman)
library(dplyr)
library(tidyverse)
library(readr)
masterid <- read_csv("C:/Users/minjk/Downloads/masterid.csv")
Master = as.data.frame(masterid)
View(Master)
table(Master$mlb_team)
colnames(Master)
# Mookie Bett's Record
Master[which(Master$retro_name == "Mookie Betts"),]

# Mookie Bett'S Batting Record 
data("Batting")
View(Batting)

data("Pitching")
head(Pitching)

#Babe Ruth Pitching Record 
Pitching[which(Pitching$playerID == "ruthba01"),]

# Fiedling Table
head(Fielding)

#Babe Ruth Fielding statistics
Fielding[which(Fielding$playerID == "ruthba01"),]


data(spa)
