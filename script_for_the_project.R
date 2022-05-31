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

#listing new structure and columns from the new data frame
colnames(all_trips)
str(all_trips)