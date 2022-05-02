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
#sum(duplicated(rt[,2])) #8264 dupes
rt <- bind_rows(rt,tweets2)
rt <- bind_rows(rt,tweets3)
rt <- bind_rows(rt,tweets4)
rt <- bind_rows(rt,tweets5)
rt <- bind_rows(rt,tweets6)
rt <- bind_rows(rt,tweets7)
rt <- bind_rows(rt,tweets8)
rt <- bind_rows(rt,tweets9)

##REMOVING DUPLICATE TWEETS
rt <- rt %>% 
  distinct(full_text, .keep_all = TRUE) #46229left.
sum(duplicated(rt[,2])) #all good!

<<<<<<< HEAD

=======
## accessing and merging weather data
weather <- read_csv("data/NYC_weather.csv")
weather2 <- read_csv("data/weather2.csv")
weather <- rbind(weather, weather2)
weather <- weather %>%
  distinct(dt, main)
>>>>>>> d3df24a07ba76f67b762ec31f50d48a151ef3ea7


## accessing, cleaning, and merging weather data
weather <- read_csv("data/NYC_weather.csv") %>% 
  mutate(day=date(dt),
         hour=hour(dt)) %>% 
  rename(weather_desc=description) %>% 
  select(-`...1`, -dt, -main)

# clean, get day and hour in rt, and get rid of more useless columns 
rt <-  rt %>%
  mutate(day=date(created_at),
         hour=hour(created_at),
         weekday=weekdays(created_at),
         period=case_when(hour <5 ~1,
                          hour <9 & hour>=5 ~2,
                          hour <13 & hour>=9 ~3,
                          hour <17 & hour>=13 ~4,
                          hour <21 & hour>=17 ~5,
                          hour <=24 & hour>=21 ~6,)) %>% 
  select(-created_at, -created_at.1, -truncated,-contributors,
         -protected, -id.1)
#joining weather data
rt <- left_join(rt,weather, by=c("day","hour"))


# topic modeling 


# sentiment analysis 


# words2vec
