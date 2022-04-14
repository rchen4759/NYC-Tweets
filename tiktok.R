devtools::install_github("benjaminguinaudeau/tiktokr")

library(tiktokr)
library(reticulate)
library(tidyverse)


use_python(py_config()$python)

tk_install()

tk_auth(cookie = '_abck=55400005C5BC98C4AFBDC392F46B4351~-1~YAAQzmNtx/FF1SOAAQAAF4ZFJAe+flSoRQzqa3Scg4FjT82Z+poga5f2uN44lhRnEBji8MnT2lpyjhvbql8i1IsoH8L1Mj3bTQEIZEol9VbZ/L3xrf1uqFXfBG0lI3iBgg/oCCrbNBLn4otEw/s6RK1+9Aw4/fMYisjq1vNk/KzKPjb5ti511nWw+git4j+twyqigx8CbTe1xC+nsOSL3fKw50vwCOslvl5Y171em7p80IDDohE21anHOtjNs/5gAvFBOJ1Zoxny0MmaXety1g2ysmJdQGVbkZeKSNvyfnh1Ye6AFy/hLdtN0svRv93xnEpMK0tk/zomHN0yxZXCB7iE0etD3sDgomqljQqQqV+P2015y5B5ruSzzc8=~-1~-1~-1; bm_sz=687298363F7C84CAA9CBE06C049D027F~YAAQzmNtx/NF1SOAAQAAF4ZFJA8NOA9ZrT+8CjYCzbIAspcLoWlJo11hubVe8hY5caU3SR6cndAvMUDt+TyW460+13PhUFOh4s4YsNBRpXQQ+ABWzOxqq1y4HvMk/aZh1L66Cavq8YZLCRdeKvm/KnRH6VaI0lne+CHmMvfaxhVc4XkshwGPAFrDli8EaJzFrOF6C5cyT+3BtWIPDAWR8Cq8rNRUFWOC65iRcij+SlpZGL5C4xPowxCfzF7cGxOrcwWQsap+glET8bKLz45hvWd+2NPF3CxQA5s+J+3TI1T8GUM=~3289652~3223620; __tea_cache_tokens_1988={%22_type_%22:%22default%22}; msToken=10gaKclWt7l_SLYpNP2YQmQkvMIqdZqIS_iWbGgSscF2PGm9ybEZigC9qZ6sZPrlfdy_ciLc9ImHFrACrv4czGcf1vLwFKxfUEKrMPPDHi7mFJh3uWvePW-hOT7I-8vZLl4vrc8=; msToken=jfGJYvCzwEEexQd-_4vuMk8Gv40K1q3sie_aOrHqG00mZPnOuHjVKaKTBquus_LDx9f25EbKjcoBtEkLZAOaWiP148rIrzHnPDukHhhQPuGTpYh2VyNXq-r7vw7lVjKzKfybulk=')

tk_init()

user_posts <- tk_posts(scope= "user", query = "willsmith", n=50)

hash_post <- tk_posts(scope="hashtag", query="maincharacter", n=1000)

charli <- tk_posts(scope="user", query="charlidamelio", n=50)
