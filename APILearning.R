library(tidyverse)
library(httr)
library(jsonlite)
library(rlist)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)
Sys.setenv(TZ="Asia/Shanghai")

starting <- "20171001"
ending <- "20171003"
article_title <- "Yes Minister"

url <- paste("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents",
             article_title,
             "daily",
             starting,
             ending,
             sep = "/")


response <-GET(url, user_agent="my@email.com this is a test")

result <- fromJSON(content(response, as="text"))

toJSON(fromJSON(content(response, as="text")), pretty = TRUE)

df <- list.stack(list.select(result, timestamp, views))

df$timestamp <- ymd(str_sub(df$timestamp, 1, -3))


get_pv <- function(article_title, starting, ending){
    url <- paste("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents",
                 article_title,
                 "daily",
                 starting,
                 ending,
                 sep = "/")
    
    df <- url %>%
        GET(user_agent="wangxuesong29@gmail.com this is a test") %>%
        content(as = "text") %>%
        fromJSON() %>%
        list.select(timestamp, views) %>%
        list.stack() %>%
        mutate(timestamp = timestamp %>%
                   str_sub(1, -3) %>%
                   ymd())
    df
    
}

starting <- "20141001"
ending <- "20171029"
article_title <- "iphone"
df <- get_pv(article_title, starting, ending)
df
df <- ggplot(data = df, aes(timestamp, views)) + geom_point()
ggplotly(df)
