## reference to this website: https://www.r-bloggers.com/2022/03/how-to-get-twitter-data-using-r/

## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
## install dev version of rtweet from github
remotes::install_github("ropensci/rtweet")

install.packages("remotes")
library(remotes)
remotes::install_github("ashoksiri/rtweet")

## load rtweet package
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

library(twitteR)

search_tweet <- searchTwitter('nyu', since='2021-03-01', until='2021-03-02')

####Research Question: Given all tweets relating to X topic, what determines the number of likes and re-tweets?###

## Siyun (words to vector)
library(text2vec)
library(purrrlyr)
library(caret)
library(glmnet)
library(ggrepel)
library(udpipe)
library(word2vec)

### loading and preprocessing a training set of tweets
# function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")


rt <- data.frame(rt)

trends <- trends_available()
trends


## plot of tweets by hour
ts_plot(rt, "hour") 


tweet_text <- rt %>%
  select(created_at, id, full_text, retweet_count, favorite_count) %>%
  dmap_at('full_text', conv_fun)

## divide tweet text into separate words
tweet_tokens <- tweet_text %>%
  select(id, full_text) %>%
  unnest_tokens(word, full_text)

x <- tolower(tweet_text$full_text)

model <- word2vec(x=x, type="cbow", dim = 15, iter=20)

embedded <- as.matrix(model)
embedded <- predict(model, c("nyc"), type = "embedding")


other <- predict(model, c("nyc"), type = "nearest", top_n=5)

#-------------------------------------------------------------------------
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



######## sentiment by bag of words#######
library(tm)
library(qdap)
tweet_text <- tweet_text %>%
  mutate(full_text = tolower(full_text))

frequency <- freq_terms(tweet_text$full_text,
                        top = 30, 
                        stopwords = c(Top100Words, "nyc"),
                        at.least = 3)

plot(frequency)

library(tm)  
# function used to clean the corpus 
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
}

# building a corpus (collection of documents)
tweets_source <- VectorSource(tweet_text$full_text)

# make tweets_corpus
tweets_corpus <- VCorpus(tweets_source)

str(tweets_corpus[[15]])

clean_corp <- clean_corpus(tweets_corpus)

clean_corp[[227]][1]

# original tweet
tweet_text$full_text[227]

# document-term matrix 
tweets_dtm <- DocumentTermMatrix(clean_corp)
tweets_m <- as.matrix(tweets_dtm)

# review portion of the matrix 
tweets_m[148:158, 10:22]

### sentiment/words ####

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

## word2vec###
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")

tweets_classified_1 <- joint_rt %>%
  select(mean_sentiment, id, created_at, text,) %>%
  dmap_at('text', conv_fun) %>%
  mutate(sentiment = ifelse(mean_sentiment == 0, 0, 1))


head(tweets_classified)

tweets_classified <- separate(tweets_classified, created_at, c("date", "time"), sep = " ") %>%
  select(-mean_sentiment)

set.seed(1234)

trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8,
                                  list = FALSE, 
                                  times = 1)

tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

# vectorization 
prep_fun <- tolower 
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text,
                   preprocessor = prep_fun,
                   tokenizer = tok_fun, 
                   ids = tweets_train$id,
                   progressbar = TRUE)

it_test <- itoken(tweets_test$text,
                  preprocessor = prep_fun,
                  tokenizer = tok_fun, 
                  ids = tweets_test$id,
                  progressbar = TRUE)

vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)

# definite tf-idf model 
tfidf <- TfIdf$new()

# fit model to train data and transform 
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)

# apply pre-trained tf-idf transformation to test data 
dtm_test_tfidf <- create_dtm(it_test, vectorizer) %>%
  transform(tfidf)

# train the word2vec model 
glmnet_class <- cv.glmnet(x = dtm_train_tfidf, 
                          y = tweets_train[['sentiment']],
                          family = 'binomial',
                          alpha = 1,
                          type.measure= "auc",
                          nfolds = 5,
                          thresh = 1e-3,
                          maxit = 1e3)

plot(glmnet_class)

preds_tweets <- predict(glmnet_class, dtm_test_tfidf, type = 'response')[,1]


# analyze sentiment by word2vec model 
df_tweets <- tweets_classified_1 

it_tweets <- itoken(df_tweets$text,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    ids = df_tweets$id,
                    progressbar = TRUE)

dtm_tweets <- create_dtm(it_tweets, vectorizer)

# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# predict probabilities of positiveness
preds_tweets <- predict(glmnet_class, dtm_tweets_tfidf, type = 'response')[ ,1]

# adding rates to initial dataset
df_tweets$sentiment <- preds_tweets

# color palette
cols <- c("#ce472e", "#f05336", "#ffd73e", "#eec73a", "#4ab04a")

set.seed(932)
samp_ind <- sample(c(1:nrow(df_tweets)), nrow(df_tweets) * 0.1) # 10% for labeling

# plotting
ggplot(df_tweets, aes(x = created_at, y = sentiment, color = sentiment)) +
  theme_minimal() +
  scale_color_gradientn(colors = cols, limits = c(0, 1),
                        breaks = seq(0, 1, by = 1/4),
                        labels = c("0", round(1/4*1, 1), round(1/4*2, 1), round(1/4*3, 1), round(1/4*4, 1)),
                        guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_point(aes(color = sentiment), alpha = 0.8) +
  geom_hline(yintercept = 0.65, color = "#4ab04a", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_hline(yintercept = 0.35, color = "#f05336", size = 1.5, alpha = 0.6, linetype = "longdash") +
  geom_smooth(size = 1.2, alpha = 0.2) +
  geom_label_repel(data = df_tweets[samp_ind, ],
                   aes(label = round(sentiment, 2)),
                   fontface = 'bold',
                   size = 2.5,
                   max.iter = 100) +
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


## clean tibble 
unnested <- rt %>% unnest(metadata)

## Duja (topic modeling)



## Rachel (sentiment analysis)




