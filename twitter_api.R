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

## load rtweet
library(rtweet)
## load the tidyverse
library(tidyverse)
library(tidytext)

## store api keys 
api_key <- '8rQ9WEkQYSE2wE5SsdBLGhfq8'
api_secret_key <- 'Nsp0Mh2o0rKnvQDzZLLn6QG6Op3yuiVcbP0aQZVMK6NvW2UonZ'

## authenticate via web browser
token <- create_token(
  app = "MDML2022",
  consumer_key = api_key,
  consumer_secret = api_secret_key)
auth_get()
auth_setup_default()
rt <- search_tweets("#NYC", n = 1000, include_rts = FALSE)
View(rt)

####Research Question: Given all tweets relating to X topic, what determines the number of likes and re-tweets?###

## Siyun (words to vector)
library(text2vec)
library(purrrlyr)
library(caret)
library(glmnet)
library(ggrepel)

### loading and preprocessing a training set of tweets
# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")


rt <- data.frame(rt)
tweet_text <- rt %>%
  select(id, full_text, retweet_count, favorite_count)

tweet_tidytext <- tweet_text %>%
  unnest_tokens(output = words_text, input=full_text, token = "words")


# select words
tweet_words_ls <- list(tweet_tidytext$words_text)
it <- itoken(tweet_words_ls, progressbar = FALSE)
tweet_vocab <- create_vocabulary(it)
tweet_vocab <- prune_vocabulary(tweet_vocab, term_count_min = 5L)

# vectorizing 
vectorizer <- vocab_vectorizer(tweet_vocab)

tweets_tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove <- GlobalVectors$new(rank = 50, x_max = 10)
tweet_wv_main = glove$fit_transform(tweets_tcm, n_iter = 100, convergence_tol = 0.00001)
wv_context = glove$components
word_vectors = tweet_wv_main + t(wv_context)

NYC = word_vectors["NYC", , drop=FALSE]

NYC_cos_sim <- sim2(x=word_vectors, y = NYC, method = "cosine", norm = "l2")
head(sort(NYC_cos_sim[,1], decreasing = TRUE), 5)



######## sentiment by word2vec#######
library(ROAuth)
df_tweets <- twListToDF(search_tweets("#NYC", n = 1000, include_rts = FALSE))

## Duja (topic modeling)



## Rachel (sentiment analysis)




