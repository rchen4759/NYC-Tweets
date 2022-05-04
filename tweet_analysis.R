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
library(rtweet)
library(tidyverse)
library(rvest)
library(twitteR)
library(Unicode)
library(tidytext)


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


###------- accessing, cleaning, and merging weather data-------------------###
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
rt <- rt_with_sentiment %>%
  group_by(text, day, hour, weekday) %>%
  mutate(mean_sentiment = mean(sentiment)) %>%
  distinct(text, day, hour, .keep_all = TRUE) %>%
  select(-word)


###########-------------------- emoji analysis----------------################

# set up emoji dictionary
emoji_dictionary <- read.csv2("../MDML_Project/data/emojis.csv") %>% 
  select(description = EN, r_encoding = ftu8, unicode)

# plain skin tones
skin_tones <- c("light skin tone", 
                "medium-light skin tone", 
                "medium skin tone",
                "medium-dark skin tone", 
                "dark skin tone")

# remove plain skin tones and info in description
emoji_dictionary <- emoji_dictionary %>%
  # remove plain skin tones emojis
  filter(!description %in% skin_tones) %>%
  # remove emojis with skin tones info, e.g. remove woman: light skin tone and only
  # keep woman
  filter(!grepl(":", description)) %>%
  mutate(description = tolower(description)) %>%
  mutate(unicode = as.u_char(unicode))
# all emojis with more than one unicode codepoint become NA 

# set up matchto and description variables 
matchto <- emoji_dictionary$r_encoding
description <- emoji_dictionary$description

# change format 
rt <- rt %>% 
  mutate(text = iconv(text, from = "latin1", to = "ascii", sub = "byte"))

# find most used emojis
rank <- emojis_matching(rt$text, matchto, description) %>% 
  group_by(description) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>%
  arrange(-n)
write_csv(rank, "../MDML_Project/data/emoji_freq_table.csv")
head(rank, 10)

# tweets with most emojis
most_emojis <- emojis_matching(rt$text, matchto, description) %>% 
  group_by(text) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>% 
  # I add the time created because it makes it easier to look up certain tweets
  merge(rt, by = "text") %>% 
  select(text, n, created_at) %>%
  arrange(-n)
write_csv(most_emojis, "../MDML_Project/data/emoji_table.csv")

mean(most_emojis$n, na.rm = TRUE)

# ---------------------------------------------------------------------------- #
# sentiment analysis

# reference website
url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

# get emoticons
emojis_raw <- url %>%
  read_html() %>%
  html_table() %>%
  data.frame() %>%
  select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", 
                       "neutral", "positive", "sentiment_score", "description", 
                       "block")

# change numeric unicode to character unicode to be able to match with emDict 
emojis <- emojis_raw %>%
  mutate(unicode = as.u_char(unicode)) %>%
  mutate(description = tolower(description)) 

str(emojis)

# unicode column is unicode character class

# merge with emDict to get encoding
emojis_merged <- emojis %>%
  merge(emoji_dictionary, by = "unicode")
# emojis %>% filter(!unicode %in% emDict$unicode) %>% View
# we loose 137 emojis that are not in emDict and for which we don't have an R encoding
# but they seem to be black and white emojis not too often used in social media anyways

new_matchto <- emojis_merged$r_encoding
new_description <- emojis_merged$description.x
sentiment <- emojis_merged$sentiment_score

# aggregated sentiment score
# higher the score, more positive the tweet 
sentiments <- emojis_matching(rt$text, new_matchto, new_description, sentiment) %>%
  mutate(sentiment = count * as.numeric(sentiment)) %>%
  group_by(text) %>% 
  summarise(sentiment_score = sum(sentiment, na.rm = TRUE))

sentiments %>% filter(sentiment_score > 0)

rt_merged <- rt %>% 
  select(text, created_at) %>% 
  merge(sentiments, by = "text", all.x = TRUE)

rt_merged %>% filter(sentiment_score > 0)
# some tweets don't have sentiment scores

# plot over time:
rt_merged %>% 
  mutate(date = as.Date(created_at)) %>% 
  group_by(date) %>% 
  summarise(sent = mean(sentiment_score, na.rm = TRUE)) %>% 
  ggplot + 
  aes(x = date, y = sent) + 
  geom_point() + 
  geom_line()


# ---------------------------------------------------------------------------- #
# emojis associated with words in tweets

# emojis for each tweet with clean text
raw_texts <- emojis_matching(rt$text, matchto, description) %>% 
  select(-sentiment, -count) %>%
  mutate(text = cleanPosts(text)) %>%
  filter(text != "") %>% 
  filter(!is.na(description))

# data frame of emojis with top words 
word_emojis <- wordFreqEmojis(raw_texts, raw_texts$text, raw_texts$description) %>% 
  filter(!is.na(words))

# ---------------------------------------------------------------------------- #

# emojis with weekdays
emojis_matching(rt$text, matchto, description) %>%
  merge(rt %>% select(text, created_at), by = "text") %>% 
  select(description, created_at) %>% 
  mutate(weekday = weekdays(created_at)) %>% 
  select(-created_at) %>% 
  group_by(weekday) %>% 
  summarise(n = n()) %>% 
  arrange(-n)





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
                        top = 20, 
                        stopwords = c(Top200Words, "nyc", "&amp", 
                                      "NYC", 'amp','newyork',
                                      "newyorkcity",'york', 
                                      'httpstco', 'httpstcocmu'),
                        at.least = 3)
plot(frequency)

freq_by_day <- rt %>% unnest_tokens(word, full_text) %>%
  select(day, word)

freq_by_day <- freq_by_day %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c("https", "nyc", "t.co","new", "york", 
                      "city", "cmu7nohbwc", "newyork")) %>%
  group_by(day) %>%
  count(word)

freq_by_day2 <- freq_by_day %>% arrange(desc(n)) %>%
  group_by(day) %>%
  slice(1:5)

word_plot <- ggplot(data=freq_by_day2, aes(x=day, y=n, colour=word)) + 
  geom_point() + ylab("Number of Words")

word_plot <- word_plot + facet_wrap(~word, ncol=3)


# sentiment plot 
ggplot(rt_with_sentiment1, aes(x = day, y = mean_sentiment, color = mean_sentiment)) +
  theme_minimal() +
  geom_point(aes(color = mean_sentiment), alpha = 0.8) +
  geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, face = "bold", color = 'black')) +
  ggtitle("Tweets Sentiment rate (probability of positiveness)")

# EMOJIS 



#RUN LINEAR REGRESSION MODEL ON TRAINING SET


# GET RMSE


# RUN ON TESTING SET


# GET RMSE
