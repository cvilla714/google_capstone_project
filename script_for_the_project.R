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