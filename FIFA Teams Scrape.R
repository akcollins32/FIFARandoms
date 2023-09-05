setwd("/Users/austin/Documents/R/FIFA Randoms")

require(rvest)
require(stringr)
require(tidyverse)
url <- "https://www.fifaindex.com/teams/fifa23/"
webpage <- read_html(url)

x <- 23 #number of pages

pages <- as.data.frame(str_c(url, "?page=", 1:x))

posts <- NULL

for (i in 1:x){
  webpage <- read_html(pages[i, ])
  posts[[i]] <- webpage %>%
    html_table() %>%
    as.data.frame()
}

url2 <- "https://www.goal.com/en-us/news/fifa-23-leagues-competitions/blt1213055127677eaf" # nolint: line_length_linter.
webpage2 <- read_html(url2)

Leagues <- webpage2 %>% html_table() %>% as.data.frame()

colnames(Leagues) <- c("League", "Country" )



all.posts <- (posts)
Teams <- as.data.frame(matrix("", nrow = 678, ncol = 6))
row <- 1
for (page in 1:23) {
  x <- as.data.frame(posts[page])
  x <- x %>%
    filter(Name != "") %>%
    .[, 2:7]
  if (page == 23){
    row = 661
    lrow = 678
    Teams[row:lrow,] <-  x
  } else {
  lrow <- nrow(x)*page
  Teams[row:lrow,] <- x
  row <-  lrow + 1
  }
}

colnames(Teams) <- c("Team", "League", "ATT", "MID", "DEF", "OVR")
Leagues <- read.csv("FIFA Leagues.csv")
Teams <- left_join(Teams, Leagues, by = "League")

Teams <- Teams %>%
  .[, c(1, 7, 2:6)]

write.csv(Leagues, "FIFA Leagues.csv", row.names = FALSE)
write.csv(Teams, "FIFA Teams.csv", row.names = FALSE)
