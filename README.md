# WhatsappChatAnalysiswithR
WhatsApp Chat Analysis with R | Data Science

## Exporting the chat

If you're a iOS user you can follow as:

1. Settings -> Chats -> Export Chat

Now you will see the list of chat with your contacts.

2. Select chats you want to export.
With or Without Media

Save the exported chat in your storage and bring it to your R Project.


## Loading libraries

```{r message=FALSE, warning=FALSE}
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

```

## Messages Per Day  Chart
Bar chart of chat messages with it's frequency or count.

```{r message=FALSE, warning=FALSE}
chatsFile <- "_chat.txt"
miChat <- rwa_read(chatsFile)
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

```

![Per day chart](https://i.imgur.com/tZARiB6.png)

## MESSAGES PER DAY OF THE WEEK

```{r message=FALSE, warning=FALSE}

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
```
![MESSAGES PER DAY OF THE WEEK](https://i.imgur.com/51WmkoQ.png)

## Most used words WordCloud

```{r}

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

```

![Most used words WordCloud](https://i.imgur.com/TC4wfzs.png)
