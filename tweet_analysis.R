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
library(topicmodels)
library(tm)
library(tidytext)
library(reshape2)
library(qdap)


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
tweets10 <- read_csv("../MDML_Project/data/CLEAN_rt_file_noh_NYC1.csv")
tweets11 <- read_csv("data/CLEAN_rt0431_18K.csv")

tweets10 <- read_csv("../MDML_Project/data/CLEAN_nyc_data.csv")
tweets11 <- read_csv("../MDML_Project/data/CLEAN_rt_file_noh_NYC1.csv")

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
rt <- bind_rows(rt,tweets10)
rt <- bind_rows(rt,tweets11)

rt <- bind_rows(rt,tweets10)
rt <- bind_rows(rt,tweets11)

##REMOVING DUPLICATE TWEETS
rt <- rt %>% 
  distinct(full_text, .keep_all = TRUE) #46343 left.
sum(duplicated(rt[,2])) #all good!


## accessing, cleaning, and merging weather data
weather <- read.csv("data/weather2.csv")
weather2 <- read_csv("data/nyc_weather.2.csv")

weather<- weather %>% 
  distinct(dt, description) %>% 
  mutate(day=date(dt),
         hour=hour(dt)) %>% 
  rename(weather_desc=description) %>% 
  select(-dt)

weather2 <- weather2 %>% 
  distinct(date, time, description) %>% 
  mutate(day=mdy(date),
         hour=hour(hms(time))) %>% 
  rename(weather_desc=description) %>% 
  select(-date, -time)

weather <- bind_rows(weather,weather2) %>% 
  distinct(day,hour, .keep_all = TRUE) 

# clean, get day and hour in rt, and get rid of more useless columns 
rt <-  rt %>%
  mutate(day=date(created_at),
         hour=hour(created_at),
         weekday=weekdays(created_at),
         period=case_when(hour <4 ~1,
                          hour <8 & hour>=4 ~2,
                          hour <12 & hour>=8 ~3,
                          hour <16 & hour>=12 ~4,
                          hour <20 & hour>=16 ~5,
                          hour <=23 & hour>=20 ~6,)) %>% 
  select(-created_at, -created_at.1, -truncated,-contributors,
         -protected, -id.1)

#joining weather data
rt <- left_join(rt,weather, by=c("day","hour"))


# TOPIC MODELING
#cleaning the corpus
##22673 25540
tm_rt <- rt %>% slice(-c(22673, 25540))
corpus <- Corpus(VectorSource(tm_rt$full_text))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, c("&amp", "nyc", 'amp','newyork',
                                        "newyorkcity",'york'))

DTM <- DocumentTermMatrix(corpus) 

ap_lda <-LDA(DTM, k = 10, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

ap_documents <- tidy(ap_lda, matrix = "gamma") 

#transforming into wide format to use as predictors
topics = ap_documents %>%
  spread(topic, gamma) %>% 
  mutate(document=row_number())

# adding the topics as predictors to tm_rt
rt <- cbind(tm_rt, topics)

# Applying the LDA on the testing set

# bags of words 
frequency <- freq_terms(rt$full_text,
                        top = 30, 
                        stopwords = c(Top100Words, "nyc", "&amp", 
                                      "nyc", 'amp','newyork',
                                      "newyorkcity",'york', 
                                      'httpstco', 'httpstcocmu'),
                        at.least = 3)
plot(frequency)

# making a term-document matrix
tweets_tdm <- TermDocumentMatrix(corpus)

# creating matrix
tweets_m <- as.matrix(tweets_tdm)

# sentiment analysis 
# split full_text column into separate words
rt2 <- rt %>% unnest_tokens(word, full_text)

# inner join with bing lexicon
rt_with_sentiment <- inner_join(rt2, get_sentiments("bing"))

# recode positive and negative sentiments as 1 or 0
rt_with_sentiment <- rt_with_sentiment %>% 
  mutate(sentiment = case_when(
    sentiment == 'positive' ~ 1,
    sentiment == 'negative' ~ 0))

# mean sentiment per tweet
rt_with_sentiment1 <- rt_with_sentiment %>%
  group_by(text, day, hour, weekday) %>%
  mutate(mean_sentiment = mean(sentiment)) %>%
  distinct(text, day, hour, .keep_all = TRUE) %>%
  select(-word)


# EMOJIS 



#RUN LINEAR REGRESSION MODEL ON TRAINING SET


# GET RMSE


# RUN ON TESTING SET


# GET RMSE
