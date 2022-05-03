# load libraries 
library(tidyverse)
library(text2vec)
library(purrrlyr)
library(caret)
library(glmnet)
library(ggrepel)
library(udpipe)
library(word2vec)
library(lubridate)

# load data from csvs extracted over a period of time
tweets <- read_csv("../MDML_Project/data/CLEAN_rt0431_18K.csv")
tweets1 <- read_csv("../MDML_Project/data/CLEAN_rt_file_10K.csv")
tweets2 <- read_csv("../MDML_Project/data/CLEAN_05.01.22-18K.csv")
tweets3 <- read_csv("../MDML_Project/data/CLEAN_rt_file_15K.csv")
tweets4 <- read_csv("../MDML_Project/data/CLEAN_rt_file_15K_2.csv")
tweets5 <- read_csv("../MDML_Project/data/CLEAN_10:35pm.csv")
tweets6 <- read_csv("../MDML_Project/data/CLEAN_10:50pm.csv")
# data from nyc
tweets7 <- read_csv("../MDML_Project/data/CLEAN_nyc_data.csv")
# data from new york city 
tweets8 <- read_csv("../MDML_Project/data/CLEAN_newyorkcity_data.csv")
tweets9 <- read_csv("../MDML_Project/data/CLEAN_newyorkcity2_data.csv")

# bind the data and remove the duplicated entries (based on full_text)
rt <- bind_rows(tweets,tweets1)
sum(duplicated(rt[,2])) #8264 dupes
rt <- bind_rows(rt,tweets2)
sum(duplicated(rt[,2])) #23304
rt <- bind_rows(rt,tweets3)
sum(duplicated(rt[,2])) #15926
rt <- bind_rows(rt,tweets4)
sum(duplicated(rt[,2])) 
rt <- bind_rows(rt,tweets5)
sum(duplicated(rt[,2])) 
rt <- bind_rows(rt,tweets6)
sum(duplicated(rt[,2])) 
rt <- bind_rows(rt,tweets7)
sum(duplicated(rt[,2])) 
rt <- bind_rows(rt,tweets8)
sum(duplicated(rt[,2])) 
rt <- bind_rows(rt,tweets9)
sum(duplicated(rt[,2])) 

rt <- rt %>% 
  distinct(full_text, .keep_all = TRUE) #46229left.
sum(duplicated(rt[,2]))

## accessing and merging weather data
weather <- read_csv("data/NYC_weather.csv")
weather2 <- read_csv("data/weather2.csv")
weather <- rbind(weather, weather2)
weather <- weather %>%
  distinct(dt, main)

weather <- separate(weather, dt, c("date", "time"), sep = " ")

# clean and only keep useful columns 
tweets <- separate(tweets, created_at, c("date", "time"), sep = " ")




# topic modeling 


# sentiment analysis 


# words2vec
