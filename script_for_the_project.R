#Adding instructions for the scrip

#Installing packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

#loading packages
library(tidyverse)
library(lubridate)
library(ggplot2)

#get my working directory
getwd()


#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files)
m1_2021 <- read.csv("202101-divvy-tripdata.csv")
m2_2021 <- read.csv("202102-divvy-tripdata.csv")
m3_2021 <- read.csv("202103-divvy-tripdata.csv")
m4_2021 <- read.csv("202104-divvy-tripdata.csv")
m5_2021 <- read.csv("202105-divvy-tripdata.csv")
m6_2021 <- read.csv("202106-divvy-tripdata.csv")
m7_2021 <- read.csv("202107-divvy-tripdata.csv")
m8_2021 <- read.csv("202108-divvy-tripdata.csv")
m9_2021 <- read.csv("202109-divvy-tripdata.csv")
m10_2021 <- read.csv("202110-divvy-tripdata.csv")
m11_2021 <- read.csv("202111-divvy-tripdata.csv")
m12_2021 <- read.csv("202112-divvy-tripdata.csv")


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
colnames(m1_2021)
colnames(m2_2021)
colnames(m3_2021)
colnames(m4_2021)
colnames(m5_2021)
colnames(m6_2021)
colnames(m7_2021)
colnames(m8_2021)
colnames(m9_2021)
colnames(m10_2021)
colnames(m11_2021)
colnames(m12_2021)


# Inspect the dataframes and look for incongruencies
str(m1_2021)
str(m2_2021)
str(m3_2021)
str(m4_2021)
str(m5_2021)
str(m6_2021)
str(m7_2021)
str(m8_2021)
str(m9_2021)
str(m10_2021)
str(m11_2021)
str(m12_2021)

# Stack individual quarter's data frames into one big data frame

all_trips <- bind_rows(m1_2021,m2_2021,m3_2021,m4_2021,m5_2021,m6_2021,m7_2021,m8_2021,m9_2021,m10_2021,m11_2021,m12_2021)

colnames(all_trips)
str(all_trips)

# Remove lat, long fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))



#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
#listing all columns from the new data frame
colnames(all_trips)

#display the structure of the new data frame
str(all_trips)

#dislay the dimensions of the data frame
dim(all_trips)

#show the first 6 rows of the data frame
head(all_trips)

#show the last 6 rows of the data frame
tail(all_trips)

#displays the statistical summary of the whole data frame.
summary(all_trips)

#checking the amount of users per labe
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
#The default format is yyyy-mm-dd
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


str(all_trips)

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)


# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


# Remove "bad" data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#several commands to run to check
#check structure of the data frame
str(all_trips_v2)

#look at the first 6 rows of the data frame
head(all_trips_v2)

#look at the last 6 rows of the data frame
tail(all_trips_v2)

#looks at column names
colnames(all_trips_v2)
