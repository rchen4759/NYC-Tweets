## reference to this website: https://www.r-bloggers.com/2022/03/how-to-get-twitter-data-using-r/

## install rtweet from CRAN
install.packages("rtweet")
## OR
## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
## install dev version of rtweet from github
remotes::install_github("ropensci/rtweet")

## load rtweet package
library(rtweet)
library(twitteR)

## load rtweet
library(rtweet)
## load the tidyverse
library(tidyverse)
## store api keys 
api_key <- 'jalqtdNQ3crKI9bNI9yM5H1Fw'
api_secret_key <- 'TNXY1ycZ5W705QH836flvCs7A43qJ2jKwM27Qumur8AnlkOOiD'

## authenticate via web browser
token <- create_token(
  app = "MDML Project",
  consumer_key = api_key,
  consumer_secret = api_secret_key)
auth_get()
auth_setup_default()
rt <- search_tweets("#NYC", n = 1000, include_rts = FALSE)
View(rt)

search_tweet <- searchTwitter('nyu', since='2021-03-01', until='2021-03-02')

####Research Question: Given all tweets relating to NYC, what determines the number of likes and retweets?###

## Siyun (words to vector)

## Duja (topic modeling)

## Rachel (sentiment analysis)
# load library
library(tidytext)
library(lubridate)

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
  group_by(id) %>%
  summarise(mean_sentiment = mean(sentiment))

joint_rt <- left_join(rt, rt_with_sentiment1, by = "id")
