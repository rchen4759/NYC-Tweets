source('library.R')


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

dist(rt$favorite_count)

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
match_emoji <- emojis_matching(rt$text, matchto, description)

rank <- match_emoji %>% 
  group_by(description) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>%
  arrange(-n)
write_csv(rank, "../MDML_Project/data/emoji_freq_table.csv")
head(rank, 10)

# tweets with most emojis
most_emojis <- match_emoji %>% 
  group_by(text) %>% 
  summarise(emoji_freq = sum(count, na.rm = TRUE)) %>% 
  merge(rt, by = "text") %>% 
  select(text, emoji_freq)

mean(most_emojis$n, na.rm = TRUE)

rt <- left_join(rt, most_emojis, by="text")

# --------------------- emojis sentiment analysis --------------------------- #
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

# merge with emDict to get encoding
emojis_merged <- emojis %>%
  merge(emoji_dictionary, by = "unicode")
# we loose 137 emojis that are not in emDict and for which we don't have an R encoding
# but they seem to be black and white emojis not too often used in social media anyways

new_matchto <- emojis_merged$r_encoding
new_description <- emojis_merged$description.x
sentiment <- emojis_merged$sentiment_score

# aggregated sentiment score
# higher the score, more positive the tweet 
sentiments <- emojis_matching(rt$text, new_matchto, new_description, sentiment) %>%
  mutate(emoji_sentiment = count * as.numeric(sentiment)) %>%
  group_by(text) %>% 
  summarise(emoji_sentiment = sum(emoji_sentiment, na.rm = TRUE))

rt <- left_join(rt, sentiments, by = "text")

# plot over time:
rt %>% 
  group_by(day) %>% 
  summarise(mean_emoji_sent = mean(emoji_sentiment, na.rm = TRUE)) %>% 
  ggplot + 
  aes(x = day, y = mean_emoji_sent) + 
  geom_point() + 
  geom_line()

#--------------------------- TOPIC MODELING ---------------------------------#
#cleaning the corpus
##22673 25540
#tm_rt <- rt %>% slice(-c(22673, 25540))
corpus <- Corpus(VectorSource(rt$text))
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
rt <- cbind(rt, topics)

# Applying the LDA on the testing set

#----------------------------- bags of words----------------------------------#
frequency <- freq_terms(rt$text,
                        top = 20, 
                        stopwords = c(Top200Words, "nyc", "&amp", 
                                      "NYC", 'amp','newyork',
                                      "newyorkcity",'york', 
                                      'httpstco', 'httpstcocmu'),
                        at.least = 3)
# top 20 words used plot
top20 <- plot(frequency)

freq_by_day <- rt %>% unnest_tokens(word, text) %>%
  select(day, word)

freq_by_day <- freq_by_day %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% c("https", "nyc", "t.co","new", "york", 
                      "city", "cmu7nohbwc", "newyork", "1",
                      "amp")) %>%
  group_by(day) %>%
  count(word)

freq_by_day2 <- freq_by_day %>% arrange(desc(n)) %>%
  group_by(day) %>%
  slice(1:4)

word_plot <- ggplot(data=freq_by_day2, aes(x=day, y=n, colour=word)) + 
  geom_point() + ylab("Number of Words")

# plot of top words used by day
word_plot <- word_plot + facet_wrap(~word, ncol=3) + theme(legend.title = element_blank()) +
  theme(legend.position = 'none') + ggtitle("Top Words Distribution by Day")

ggsave("figures/word_plot.png", width = 10, height = 10)

# cleaning final dataframe
rt <- rt %>%
  ungroup() %>%
  select(-day)
  #select(-result_type, -in_reply_to_screen_name, -possibly_sensitive, -location, 
         #-description, -sentiment, -document, -text)

sum(is.na(rt))

# exporting the final data to run our regression models to save computation power
write.csv(rt, "data/tweet_model.csv")

rt <- read.csv("data/tweet_model.csv")

rt <- rt %>% select(-X)

rt <- rt %>%
  mutate(period = as.factor(period))

# data for favorite model 
rt_favorite <- rt %>%
  select(-day, -retweet_count, -hour, -X10)

# data for re-tweet model
rt_retweet <- rt %>%
  select(-day, -favorite_count, -hour, -X10)

#------------------------ predicting favorites -------------------------------#
# split training set 60%
smp_size <- floor(0.60 * nrow(rt_favorite))

## set the seed to make your partition reproducible
set.seed(123)

train <- sample(seq_len(nrow(rt_favorite)), size = smp_size)

# training set 60%
training_favorite <- rt_favorite[train, ]
other_favorite <- rt_favorite[-train, ]

half <- floor(0.50 * nrow(other))

other_fav_set <- sample(seq_len(nrow(other_favorite)), size = half)

# validation set 20%
validation_favorite <- other_favorite[other_fav_set,]

# testing set 20% 
testing_favorite <- other_favorite[-other_fav_set, ]

# linear regression 
train_fav_model <-lm(data = training_favorite, favorite_count ~ .)
summary(train_fav_model)
plot(train_fav_model, which = 1)

# RMSE on training set
train_RMSE_lmfav <- sqrt(mean(train_fav_model$residuals^2))

# RMSE on validation set
predict_fav_lm <- predict(train_fav_model, validation_favorite, type = "response")
RMSE_lm_fav <- sqrt(mean((validation_favorite$favorite_count - predict_fav_lm)^2))

# poisson regression 
train_fav_poss <-glm(data = training_favorite, favorite_count ~ ., family="poisson")
summary(train_fav_poss)
plot(train_fav_poss, which = 1)

jtools::summ(train_fav_poss, exp = T)

# training poisson RMSE
train_poss_RMSE <- sqrt(mean((training_favorite$favorite_count - train_fav_poss$fitted.values)^2))

# validation poisson RMSE
predict_fav_poss <- predict(train_fav_poss, validation_favorite, type = "response")
val_poss_RMSE <- sqrt(mean((validation_favorite$favorite_count - predict_fav_poss)^2))

## random forest 
library(ranger)
r_model <- ranger(data = training_favorite, favorite_count ~ .,
       num.trees = 1000, respect.unordered.factors = T, probability = F)

# RMSE train random forest
r_pred <- predict(r_model, data=training_favorite)$predictions

rf_fav_trainRMSE <- sqrt(mean((training_favorite$favorite_count - r_pred)^2))

# RMSE validation random forest
r_pred_val <- predict(r_model, data=validation_favorite)$predictions

rf_fav_valRMSE <- sqrt(mean((validation_favorite$favorite_count - r_pred_val)^2))

############ model that replace 0 with 0.01 ###########################
rt_0_train <- training_favorite %>%
  mutate(favorite_count = replace(favorite_count, favorite_count == 0, 0.01)) %>%
  mutate(favorite_count = log(favorite_count))

rt_0_val <- validation_favorite %>%
  mutate(favorite_count = replace(favorite_count, favorite_count == 0, 0.01)) %>%
  mutate(favorite_count = log(favorite_count))

# linear regression w/ 0.01 and log
train_model0_fav <-lm(data = rt_0_train, favorite_count ~ .)
summary(train_model0_fav)
plot(train_model0_fav, which = 1)

# GET RMSE
rt_0fav_trainRMSE <- sqrt(mean((rt_0_train$favorite_count - train_model0_fav$fitted.values)^2))

# RUN ON VALIDATION
predict_0fav <- predict(train_model0_fav, rt_0_val, type = "response")

# Validation RMSE
rt_0fav_valRMSE <- sqrt(mean((rt_0_val$favorite_count - predict_0fav)^2))

train_RMSE_lmfav
RMSE_lm_fav
train_poss_RMSE
val_poss_RMSE
rf_fav_trainRMSE
rf_fav_valRMSE
rt_0fav_trainRMSE 
rt_0fav_valRMSE

training_RMSE <- data.frame(rbind(train_RMSE_lmfav, train_poss_RMSE, rf_fav_trainRMSE, rt_0fav_trainRMSE))

validation_RMSE <- data.frame(rbind(RMSE_lm_fav, val_poss_RMSE, rf_fav_valRMSE, rt_0fav_valRMSE))

RMSE_table <- cbind(training_RMSE, validation_RMSE)

colnames(RMSE_table) <- c("Training Set","Validation Set")
rownames(RMSE_table) <- c("Linear Regression", "Poisson Regression",
                          "Random Forest", "Log Model")

write.csv(RMSE_table, "figures/RMSE_table.csv")

# Poison model selected because it was the best for our questions 
# Running poison model on testing set 
predict_fav_poss_test <- predict(train_fav_poss, testing_favorite, type = "response")
val_poss_RMSE_test <- sqrt(mean((testing_favorite$favorite_count - predict_fav_poss_test)^2))
# 212 
