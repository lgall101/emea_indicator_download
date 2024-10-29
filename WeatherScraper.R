

install.packages("rvest")

library(httr)
library(rvest)

url <- "https://www.climameter.org/hazard-database"
page <- read_html(url)

ht1 <- page %>% 
  html_nodes(".aw5Odc") %>% 
  html_text()

ht2 <- page %>%  # USE THIS ONE
  html_nodes("a.XqQF9c") %>% 
  html_text()

ht_link <- page %>% 
  html_nodes("a.XqQF9c") %>% 
  html_attr('href')

test1 <- as.data.frame(ht1)

test2 <- as.data.frame(ht2)

