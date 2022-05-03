####Research Question: Given all tweets relating to NYC, what determines the number of likes and retweets?###

## reference to this website: https://www.r-bloggers.com/2022/03/how-to-get-twitter-data-using-r/

## install rtweet from CRAN
# install.packages("rtweet")
## OR
## install remotes package if it's not already
# if (!requireNamespace("remotes", quietly = TRUE)) {
  # install.packages("remotes")
# }
## install dev version of rtweet from github
# remotes::install_github("ropensci/rtweet")

# load packages 
library(rtweet)
library(tidyverse)
library(rvest)
library(twitteR)
library(Unicode)
library(tidytext)

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

# ---------------------------------------------------------------------------- #
## PULL TWEETS AND CLEAN DATA ##

rt_noh_NYC2 <- search_tweets("NYC", n = 1000, include_rts = FALSE)
View(rt)

# collect user data
users_rt_noh_NYC2 <- users_data(rt_noh_NYC2)
users_rt_noh_NYC2 <- users_rt_noh_NYC2 %>% 
  select(location, description, protected, followers_count, friends_count,
         listed_count, created_at, verified, statuses_count)

##MERGING TWEET DATA WITH USER DATA
rt_noh_NYC2 <- cbind(rt_noh_NYC2, users_rt_noh_NYC2)

# cleaning data
#unnest metadata and filter out non-english tweets
rt_noh_NYC2 <- rt_noh_NYC2 %>% 
  unnest(metadata) %>% 
  filter(iso_language_code=="en")

## TURNING ENTITIES COLUMN INTO USABLE INFO

hashtags = sapply(rt_noh_NYC2$entities,"[",1)
rt_noh_NYC2$hashtags <- sapply(hashtags, nrow)

user_mentions = sapply(rt_noh_NYC2$entities,"[",3)
rt_noh_NYC2$user_mentions <- sapply(user_mentions, nrow) # DEAL WITH NAs
rt_noh_NYC2$user_mentions_adj=c()
for (i in 1:nrow(rt_noh_NYC2)) {
  rt_noh_NYC2$user_mentions_adj[i]=as.numeric(is.na(user_mentions[[i]][1]))
}
rt_noh_NYC2$user_mentions = rt_noh_NYC2$user_mentions-rt_noh_NYC2$user_mentions_adj


urls = sapply(rt_noh_NYC2$entities,"[",4)
rt_noh_NYC2$urls <- sapply(urls, nrow) # DEAL WITH NAs
rt_noh_NYC2$urls_adj=c()
for (i in 1:nrow(rt_noh_NYC2)) {
  rt_noh_NYC2$urls_adj[i]=as.numeric(is.na(urls[[i]][1]))
}
rt_noh_NYC2$urls = rt_noh_NYC2$urls-rt_noh_NYC2$urls_adj

media = sapply(rt_noh_NYC2$entities,"[",5)
rt_noh_NYC2$media <- sapply(media, nrow)
rt_noh_NYC2$media_adj=c()
for (i in 1:nrow(rt_noh_NYC2)) {
  rt_noh_NYC2$media_adj[i]=as.numeric(is.na(media[[i]][1]))
}
rt_noh_NYC2$media = rt_noh_NYC2$media-rt_noh_NYC2$media_adj

##DROPPING UNNECESSARY COLUMNS
rt_noh_NYC2 <- rt_noh_NYC2 %>% 
  select(-id, -id_str, -entities, -iso_language_code, -source,
         -geo, -coordinates, -place, -favorited, -retweeted,
         -lang, -quoted_status_id, -quoted_status_id_str, 
         -quoted_status, -favorited_by, -scopes, -display_text_width,
         -retweeted_status, -quoted_status_permalink, -query,
         -withheld_copyright,-withheld_in_countries, -withheld_scope,
         -possibly_sensitive_appealable, -user_mentions_adj, -urls_adj,
         -media_adj, -in_reply_to_status_id, -in_reply_to_status_id_str,
         -in_reply_to_user_id, -in_reply_to_user_id_str)

# save data
rt_file_noh_NYC2 <- data.frame(apply(rt_noh_NYC2,2,as.character))
write_excel_csv(rt_file_noh_NYC2, "Documents/A3SR/GE 2047 MDML/PROJECT/rt_file_noh_NYC2.csv")
# file should have 25 variables

# ---------------------------------------------------------------------------- #
# FUNCTIONS FOR ANALYSIS

# for emoji analysis
# function to output emojis found and number of occurrences
count_matches <- function(string, matchto, description, sentiment = NA) {
  
  vec <- str_count(string, matchto)
  matches <- which(vec != 0)
  
  descr <- NA
  cnt <- NA
  
  if (length(matches) != 0) {
    
    descr <- description[matches]
    cnt <- vec[matches]
    
  } 
  
  df <- data.frame(text = string, description = descr, count = cnt, sentiment = NA)
  
  if (!is.na(sentiment) && length(sentiment[matches]) != 0) {
    
    df$sentiment <- sentiment[matches]
    
  }
  
  return(df)
  
}

# function to apply count_matches on vector of texts and returns data frame
emojis_matching <- function(texts, matchto, description, sentiment = NA) {
  
  texts %>% 
    map_df(count_matches, 
           matchto = matchto, 
           description = description, 
           sentiment = sentiment)
  
}

# emoji word association analysis
# tweets cleaning function
cleanPosts <- function(text) {
  clean_texts <- text %>%
    gsub("<.*>", "", .) %>% # remove emojis
    gsub("&amp;", "", .) %>% # remove &
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
    gsub("@\\w+", "", .) %>% # remove at people
    hashgrep %>%
    gsub("[[:punct:]]", "", .) %>% # remove punctuation
    gsub("[[:digit:]]", "", .) %>% # remove digits
    gsub("http\\w+", "", .) %>% # remove html links
    iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
    gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
    gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
    tolower
  return(clean_texts)
}

# function that separates capital letters hashtags
hashgrep <- function(text) {
  hg <- function(text) {
    result <- ""
    while(text != result) {
      result <- text
      text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", text, perl = TRUE)
    }
    return(text)
  }
  unname(sapply(text, hg))
}

# function that outputs a df of emojis with their top 5 words (by frequency)
wordFreqEmojis <- function(df, text = df$text, description = df$description, top = 5) {
  
  map_df(unique(description), function(x) {
    
    dat <- df %>% 
      filter(description == x)
    
    myCorpus <- Corpus(VectorSource(dat$text)) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(myCorpus)
    # find the sum of words in each Document
    rowTotals <- apply(dtm , 1, sum)
    dtm.new   <- dtm[rowTotals> 0, ]
    # collapse matrix by summing over columns
    freq <- colSums(as.matrix(dtm))
    # create sort order (descending)
    ord <- order(freq, decreasing = TRUE)
    
    list(emoji = rep(x, top), 
         words = names(freq[ord][1:top]), 
         frequency = freq[ord][1:top]) 
    
  })
  
}

# ---------------------------------------------------------------------------- #
# ANALYSIS

## Siyun (words to vector)

## Duja (topic modeling)

## Rachel (sentiment analysis)

# split full_text column into separate words
rt_words <- rt %>% unnest_tokens(word, full_text)

# inner join with bing lexicon
rt_with_sentiment <- inner_join(rt_words, get_sentiments("bing"))

# recode positive and negative sentiments as 1 or 0
rt_with_sentiment <- rt_with_sentiment %>% 
  mutate(sentiment = case_when(
    sentiment == 'positive' ~ 1,
    sentiment == 'negative' ~ 0))

# mean sentiment per tweet
mean_sentiment <- rt_with_sentiment %>%
  group_by(id) %>%
  summarize(mean_sentiment = mean(sentiment)) %>% 
  mutate(id = as.character(id))

# top words
top_words <- rt_with_sentiment %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(n > 10)

# ---------------------------------------------------------------------------- #
# extract metadata 
# rt %>% unnest(quoted_status)
rt %>% unnest(metadata) %>% 
  print(width = Inf)

# ---------------------------------------------------------------------------- #
# emoji analysis

# set up emoji dictionary
emoji_dictionary <- read.csv2("Documents/A3SR/GE 2047 MDML/PROJECT/emojis.csv") %>% 
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
rt <- rt_noh_NYC2 %>% 
  mutate(text = iconv(text, from = "latin1", to = "ascii", sub = "byte"))

# find most used emojis
rank <- emojis_matching(rt$text, matchto, description) %>% 
  group_by(description) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>%
  arrange(-n)

head(rank, 10)

# tweets with most emojis
most_emojis <- emojis_matching(rt$text, matchto, description) %>% 
  group_by(text) %>% 
  summarise(n = sum(count, na.rm = TRUE)) %>% 
  # I add the time created because it makes it easier to look up certain tweets
  merge(rt, by = "text") %>% 
  select(text, n, created_at) %>%
  arrange(-n)

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




