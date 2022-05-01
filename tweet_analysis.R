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

# load data from csv 
tweets <- read_csv("rtdata0430.csv")
weather <- read_csv("NYC_weather.csv")

# clean and only keep useful columns 
tweets <- separate(tweets, created_at, c("date", "time"), sep = " ")

# add in weather data from openweather API



# topic modeling 


# sentiment analysis 


# words2vec
