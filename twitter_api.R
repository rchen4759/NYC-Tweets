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
rt <- search_tweets("#Ukraine", n = 1000, include_rts = FALSE)
View(rt)

