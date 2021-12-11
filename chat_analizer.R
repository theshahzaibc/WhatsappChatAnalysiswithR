library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(wordcloud)
library(stringr)

miChat <- rwa_read("_chat.txt")
miChat <- miChat %>% 
  mutate(day = date(time)) %>% 
  mutate(
    station = case_when(
      day >= dmy(1634037100) ~ "Till Now")
  ) %>% 
  mutate( station = factor(station) ) %>% 
  filter(!is.na(author))

palette.stations <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]
miChat %>% 
  group_by(station) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=station)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=palette.stations) +
  ylab("Number of messages") + xlab("Date") +
  ggtitle("Messages per day", "Frequency by season of the year") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")


## MESSAGES PER DAY OF THE WEEK
miChat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(station, wday.num, wday.name) %>% 
  count() %>% 
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=station)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=palette.stations) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages per day of the week", "Frequency by season of the year
") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")


# wordcloud
text <- readLines("_chat.txt")
text=str_replace_all(text,"[^[:graph:]]", " ") 
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
