library(tidyverse)
library(tesseract)
library(tidytext) 
library(readtext)
library(ggplot2)
library(spacyr)
library(ggmap)
library(tidygeocoder)
library(stringr)
library(dplyr)


#### Data Exploratory Analyses ####
#trying out different things

#store character vector and numeric vector of months of the fair
months.numeric <- c(4, 5, 6, 7, 8, 9, 10, 11, 12)
months.character <- c("April", "May", "June", "July", "August", "September", "October", "November", "December")

### plot term frequency of terms "savage" and "native" ###
token.data <- read.csv("data/tokenized_data.csv")
token.data %>%
  filter(word %in% c("savage", "native")) %>%
  group_by(month, word) %>% 
  summarise(count = n()) %>% 
  mutate(color = ifelse(word == "savage", "red4","orange3")) %>% 
  ggplot(aes(month, count, fill = color)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = c("orange3" = "orange3",
                               "red4" = "red4"),
                    labels = c("savage", "native"),
                    name = "legend") +
  xlab("month") +
  scale_x_discrete(name = "month", limits = months.numeric,
                   labels = c("4" = "April", "5" = "May", "6" = "June", "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December")) +
  ylab("count") +
  ggtitle("Frequency of terms 'savage' and 'native' in newspaper articles across the months")


### frequency of positive and negative terms used in the articles ###
#this is based on sentiment analysis methodologies, which is highly problematic for historical analysis. it is used here cautiously and merely as an added form of exploring the data to inspire new questions.

#get positive-negative terms dataset
sent <- get_sentiments(lexicon = "afinn")

#create a sentiment dataset
sent.data <- token.data %>% 
  left_join(sent) 

sent.data <- sent.data %>% 
  mutate(sentiment = ifelse((sent.data$value > 0),"positive", 
                            ifelse(sent.data$value <0, "negative", "NA")))

#plot positive and negative terms over the months
sent.data %>% 
  group_by(article_id, month) %>% 
  tally(mean(value, na.rm = TRUE)) %>%
  arrange(month) %>% 
  mutate(color = ifelse((n > 0), "green4","red4")) %>% 
  ggplot() + 
  geom_col(aes(x = month, y = n, fill = color)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = c("green4" = "green4",
                               "red4" = "red4"),
                    labels = c("positive", "negative"),
                    name = "legend") +
  scale_x_discrete(name = "months", limits = months.numeric,
                   labels = c("4" = "April", "5" = "May", "6" = "June", "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December")) +
  xlab("value") +
  ggtitle("Positive and negative terms across each month")


#plot positive and negative terms across every article
sent.data %>% 
  group_by(article_id, month) %>% 
  tally(mean(value, na.rm = TRUE)) %>%
  arrange(month) %>% 
  mutate(color = ifelse((n > 0), "green4","red4")) %>% 
  ggplot() + 
  geom_col(aes(x = article_id, y = n, fill = color)) +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green4" = "green4",
                               "red4" = "red4"),
                    labels = c("positive", "negative"),
                    name = "legend") +
  xlab("articles (April - December)") +
  ylab("value") +
  ggtitle("Positive and negative words across each newspaper article between April and December, 1904")
