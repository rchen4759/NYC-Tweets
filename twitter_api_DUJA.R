####Research Question: Given all tweets relating to X topic, what determines the number of likes and retweets?###


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

## load rtweet
library(rtweet)
## load the tidyverse
library(tidyverse)
## store api keys 
api_key <- '7xfxPgYzJPL6HpHwn4KTkVKUN'
api_secret_key <- '7kSOW4EsfgbhpIxUcTUSZaEs4FTlojkhq9RUPSK5LHYwyhInzb'

## authenticate via web browser
token <- create_token(
  app = "Duja",
  consumer_key = api_key,
  consumer_secret = api_secret_key)
auth_get()
auth_setup_default()

##EXTRACTING DATA WITH NYC HASHTAG
rt_31 <- search_tweets("#NYC", n = 18000, include_rts = FALSE)

##GETTING AND CLEANING USER INFO

users <- users_data(rt_31) %>% 
  select(location, description, protected, followers_count, friends_count,
         listed_count, created_at, verified, statuses_count)

##MERGING TWEET DATA WITH USER DATA
rt_31 <- cbind(rt_31,users)



####CLEANING RT

#unnest metadata and filter out non-english tweets
rt <- rt_31 %>% 
  unnest(metadata) %>% 
  filter(iso_language_code=="en")
# rt <- rt %>% 
#   unnest(geo, names_repair = "minimal") 
# rt <- rt %>% 
#   unnest(quoted_status=, names_repair = "minimal")


## TURNING ENTITIES COLUMN INTO USABLE INFO

hashtags=sapply(rt$entities,"[",1)
rt$hashtags <- sapply(hashtags, nrow)

user_mentions=sapply(rt$entities,"[",3)
rt$user_mentions <- sapply(user_mentions, nrow) # DEAL WITH NAs
rt$user_mentions_adj=c()
for (i in 1:nrow(rt)) {
  rt$user_mentions_adj[i]=as.numeric(is.na(user_mentions[[i]][1]))
}
rt$user_mentions=rt$user_mentions-rt$user_mentions_adj


urls=sapply(rt$entities,"[",4)
rt$urls <- sapply(urls, nrow) # DEAL WITH NAs
rt$urls_adj=c()
for (i in 1:nrow(rt)) {
  rt$urls_adj[i]=as.numeric(is.na(urls[[i]][1]))
}
rt$urls=rt$urls-rt$urls_adj

media=sapply(rt$entities,"[",5)
rt$media <- sapply(media, nrow)
rt$media_adj=c()
for (i in 1:nrow(rt)) {
  rt$media_adj[i]=as.numeric(is.na(media[[i]][1]))
}
rt$media=rt$media-rt$media_adj

##DROPPING UNNECESSARY COLUMNS
rt <- rt %>% 
  select(-id, -id_str, -entities, -iso_language_code, -source,
         -geo, -coordinates, -place, -favorited, -retweeted,
         -lang, -quoted_status_id, -quoted_status_id_str, 
         -quoted_status, -favorited_by, -scopes, -display_text_width,
         -retweeted_status, -quoted_status_permalink, -query,
         -withheld_copyright,-withheld_in_countries, -withheld_scope,
         -possibly_sensitive_appealable, -user_mentions_adj, -urls_adj,
         -media_adj, -in_reply_to_status_id, -in_reply_to_status_id_str,
         -in_reply_to_user_id, -in_reply_to_user_id_str)

##SAVING TIBBLE
write_csv(rt, "../MDML_Project/rt0431_18K.csv")
###---------------------------------------------------------------------##

## Duja (topic modeling)
#install.packages('cld3')
library(cld3)
#keeping only english languate tweets
# rt <- rt %>% 
#   mutate(lang= detect_language(full_text)) %>% 
#   filter(lang=='en')
#topic modelling
#install.packages('topicmodels')
library(topicmodels)
#install.packages('tm')
library(tm)
#install.packages("tidytext")
library(tidytext)
#install.packages("reshape2")
library(reshape2)

corpus <- Corpus(VectorSource(rt$full_text))
corpus <- tm_map(corpus, removeWords, c("#NYC","#nyc", stopwords("english")))
DTM <- DocumentTermMatrix(corpus)

ap_lda <-LDA(DTM, k = 5, control = list(seed = 1234))
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

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





