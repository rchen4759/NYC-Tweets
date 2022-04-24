# sentiment analysis on Russia invasion of Ukraine

library(rvest)
library(tidyverse)
library(tidytext)

# Fox News
# read url
url_fox1 <- 'https://www.foxnews.com/politics/ukraine-prime-minister-congress-requests-more-help-against-russia'
fox_ukraine <- read_html(url_fox1)


# extract text
fox_ukraine_text <- html_nodes(x = fox_ukraine, 
                           xpath = '//p[@class = "speakable"]')
fox_ukraine_text <- html_text(fox_ukraine_text, trim = T)

fox_ukraine_text1 <- tibble(fox_ukraine_text)

fox_text <- fox_ukraine_text1 %>%
  unnest_tokens(output = words_text, input=fox_ukraine_text, token = "words")

# sentiment analysis #

# CBS
# read url
url_cbs <- "https://www.cbsnews.com/news/russian-offensive-eastern-ukraine-zelensky/"
cbs <- read_html(url_cbs)

# extract text
cbs_text <- html_nodes(x = cbs,
                       xpath = '//p')
cbs_text <- html_text(cbs_text, trim = T)

# NY Times
# read url
url_nyt <- 'https://www.nytimes.com/2022/04/15/technology/russia-media-fox-news.html'
nyt <- read_html(url_nyt)

# extract text
nyt_text <- html_nodes(x = nyt,
                       xpath = '//p')
nyt_text <- html_text(nyt_text, trim = T)
