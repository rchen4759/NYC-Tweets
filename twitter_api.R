## reference to this website: https://www.r-bloggers.com/2022/03/how-to-get-twitter-data-using-r/

## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
## install dev version of rtweet from github
remotes::install_github("ropensci/rtweet")

#install.packages("remotes")
library(remotes)
remotes::install_github("ashoksiri/rtweet")

## load rtweet package
library(rtweet)
#library(twitteR)

## load the tidyverse
library(tidyverse)
library(tidytext)

## store api keys 
api_key <- '1Wkz19N5QhyUk2HLFOCVJhxuK'
api_secret_key <- 'ArnlXexbu80oU4ZmtxOxo4GtvtQq9LKS85o9xPOjHo5LeSIytd'
#access_token <- '1516117979804672015-vYYI6UcNpfc1hiUk9j97mhqCZUVomk'
#access_secret <- '41iggfv9NZ4ANg9QLQpCJVs5IXuaiCChrtnb7p3y3kCP2'

## authenticate via web browser
token <- create_token(
  app = "MDML2022",
  consumer_key = api_key,
  consumer_secret = api_secret_key)
auth_get()
auth_setup_default()
tweets2 <- search_tweets("#NYC", n = 18000, include_rts = FALSE)

users <- users_data(tweets2) %>% 
  select(location, description, protected, followers_count, friends_count,
         listed_count, created_at, verified, statuses_count)

##MERGING TWEET DATA WITH USER DATA
tweets_user <- cbind(tweets2,users)

#unnest metadata and filter out non-english tweets
tweets2 <- tweets_user %>% 
  unnest(metadata) %>% 
  filter(iso_language_code=="en")

## TURNING ENTITIES COLUMN INTO USABLE INFO

hashtags=sapply(tweets2$entities,"[",1)
tweets2$hashtags <- sapply(hashtags, nrow)

user_mentions=sapply(tweets2$entities,"[",3)
tweets2$user_mentions <- sapply(user_mentions, nrow) # DEAL WITH NAs
tweets2$user_mentions_adj=c()
for (i in 1:nrow(tweets2)) {
  tweets2$user_mentions_adj[i]=as.numeric(is.na(user_mentions[[i]][1]))
}
tweets2$user_mentions=tweets2$user_mentions-tweets2$user_mentions_adj


urls=sapply(tweets1$entities,"[",4)
tweets2$urls <- sapply(urls, nrow) # DEAL WITH NAs
tweets2$urls_adj=c()
for (i in 1:nrow(tweets1)) {
  tweets2$urls_adj[i]=as.numeric(is.na(urls[[i]][1]))
}
tweets2$urls=tweets2$urls-tweets2$urls_adj

media=sapply(tweets2$entities,"[",5)
tweets2$media <- sapply(media, nrow)
tweets2$media_adj=c()
for (i in 1:nrow(rt)) {
  tweets1$media_adj[i]=as.numeric(is.na(media[[i]][1]))
}
tweets2$media=tweets2$media-tweets2$media_adj

##DROPPING UNNECESSARY COLUMNS
tweets2 <- tweets2 %>% 
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
write_csv(tweets2, "../MDML_Project/CLEAN_05.01.22-18K.csv")

# REPEAT to extract more data
rt8 <- search_tweets("#nyc", n = 18000, include_rts = FALSE)

rt7 <- search_tweets("new york city", n = 18000, include_rts = FALSE)

##GETTING AND CLEANING USER INFO

users7 <- users_data(rt7) 
users7 <- users7 %>% 
  select(id, location, description, protected, followers_count, friends_count,
         listed_count, created_at, verified, statuses_count)

rt_7 <- cbind(rt7,users7)

rt7 <- rt_7 %>% 
  unnest(metadata) %>% 
  filter(iso_language_code=="en")

hashtags=sapply(rt7$entities,"[",1)
rt7$hashtags <- sapply(hashtags, nrow)

user_mentions=sapply(rt7$entities,"[",3)
rt7$user_mentions <- sapply(user_mentions, nrow) # DEAL WITH NAs
rt7$user_mentions_adj=c()
for (i in 1:nrow(rt7)) {
  rt7$user_mentions_adj[i]=as.numeric(is.na(user_mentions[[i]][1]))
}
rt7$user_mentions=rt7$user_mentions-rt7$user_mentions_adj


urls=sapply(rt7$entities,"[",4)
rt7$urls <- sapply(urls, nrow) # DEAL WITH NAs
rt7$urls_adj=c()
for (i in 1:nrow(rt)) {
  rt7$urls_adj[i]=as.numeric(is.na(urls[[i]][1]))
}
rt7$urls=rt7$urls-rt7$urls_adj

media=sapply(rt7$entities,"[",5)
rt7$media <- sapply(media, nrow)
rt7$media_adj=c()
for (i in 1:nrow(rt7)) {
  rt7$media_adj[i]=as.numeric(is.na(media[[i]][1]))
}
rt7$media=rt7$media-rt7$media_adj

##DROPPING UNNECESSARY COLUMNS
rt7 <- rt7 %>% 
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
write_csv(rt7, "../MDML_Project/data/CLEAN_newyorkcity2_data.csv")
