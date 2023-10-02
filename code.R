library(tidyverse)
library(tesseract)
library(tidytext) 
library(readtext)
library(ggplot2)
library(spacyr)

#set working directory
setwd(dir = "~/Desktop/Ph.D. Digital History/4. FALL 2023/HIST 8550 Seminar in Digital History")

#### TESTING Tesseract ####

#establish tesseract language parameters
english <- tesseract("eng")

#list set of test files in a new variable 
#testjpgs <- list.files("testocr_folder/")
#store relative path of test files in a new variable
#path <- paste("testocr_folder/")
#store relative path for output txt files in a new variable
#newdir <- paste("txt_files/")

#test loop and OCR
#for (i in 1:length(testjpgs)) {
#  filename <- paste(testjpgs[i]) #catch the file's name
#  fullpath <- paste0(path, filename) #store the file's relative path
#  result <- tesseract::ocr(fullpath) #use the file's relative path to OCR
#  print(result) #show me what's going on
#  write.table(result, file = paste0(newdir, filename, ".txt"), sep = "\t") #write a new txt file in new txt_files directory
#} 
#THIS WORKED!

#### Tesseract ####

## applying loop structure to actual JPG files

#list all jpg files in a new variable 
jpgs <- list.files("Newspaper_articles/")
#store relative path of test files in a new variable
path <- paste("Newspaper_articles/")
#store relative path for output txt files in a new variable
newdir <- paste("txt_files/")

#loop and OCR
for (i in 1:length(jpgs)) {
  filename <- paste(jpgs[i]) #catch the file's name
  fullpath <- paste0(path, filename) #store the file's relative path
  result <- tesseract::ocr(fullpath) #use the file's relative path to OCR
  print(filename) #show me what's going on
  write_file(result, file = paste0(newdir, filename, ".txt")) #write a new txt file in new txt_files directory
} 

#### Data Preprocessing #####

# building a corpus
txt.files <- readtext(file.path("txt_files/"))
txt.files <- txt.files %>% 
  arrange(doc_id)

metadata <- read.csv("TextAnalysis_StLouisFair/metadata.csv")
metadata$doc_id <- paste0(metadata$doc_id, metadata$file_format)
metadata <- metadata[,-2]
metadata <- metadata %>% 
  arrange(doc_id)

raw.data <- metadata %>% 
  left_join(txt.files, by = "doc_id")

write.csv(raw.data, file = "TextAnalysis_StLouisFair/raw_data.csv")

## tokenization

#customize stop words list
stopwords <- stop_words

stop_words_custom <- stop_words %>% 
  add_row(word="fair", lexicon="NA") %>% 
  add_row(word="world", lexicon="NA") %>% 
  add_row(word="world's ", lexicon="NA") %>% 
  add_row(word="world's", lexicon="NA") %>% 
  add_row(word="exhibit", lexicon="NA") %>% 
  add_row(word="exposition", lexicon="NA") %>% 
  add_row(word="louisiana", lexicon="NA") %>% 
  add_row(word="purchase", lexicon="NA") %>% 
  add_row(word="st", lexicon="NA") %>% 
  add_row(word="louis", lexicon="NA")

#remove stop words
data <- raw.data %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words_custom)

write.csv(data, file = "TextAnalysis_StLouisFair/tokenized_data.csv")

#### Data Exploratory Analyses ####
#trying out different things

#store character vector and numeric vector of months of the fair
months.numeric <- c(4, 5, 6, 7, 8, 9, 10, 11, 12)
months.character <- c("April", "May", "June", "July", "August", "September", "October", "November", "December")

#raw count of words
word.count <- data %>%
  count(word, sort = TRUE) %>% 
  as.data.frame()

#random term frequency test
count.indian.war <- data %>%
  filter(word %in% c("indian", "war")) %>%
  group_by(month, word) %>% 
  summarise(count = n()) %>% 
  mutate(color = ifelse(word == "indian", "orange","red"))

#plot results
count.indian.war %>% 
  ggplot(aes(month, count, fill = color)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = c("orange" = "orange",
                               "red" = "red"),
                    labels = c("indian", "war"),
                    name = "legend") +
  xlab("month") +
  scale_x_discrete(name = "month", limits = months.numeric,
                   labels = c("4" = "April", "5" = "May", "6" = "June", "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December")) +
  ylab("count") +
  ggtitle("Frequency of terms 'Indian' and 'War' in newspaper articles across the months")

#get positive-negative terms dataset
sent <- get_sentiments(lexicon = "afinn")

#plot positive and negative terms over the months
plotdata.months <- data %>% 
  inner_join(sent) #%>% 
  group_by(doc_id, month) %>% 
  tally(mean(value)) %>%
  arrange(month) %>% 
  mutate(color = ifelse((n > 0), "green","red"))

plotdata.months %>% 
  ggplot() + 
  geom_col(aes(x = month, y = n, fill = color)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values = c("green" = "green",
                               "red" = "red"),
                    labels = c("positive", "negative"),
                    name = "legend") +
  scale_x_discrete(name = "months", limits = months.numeric,
                   labels = c("4" = "April", "5" = "May", "6" = "June", "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December")) +
  xlab("value") +
  ggtitle("Positive and negative terms across each month")

#plot positive and negative terms across every txt file
plotdata.files <- data %>% 
  inner_join(sent) #%>% 
  group_by(doc_id, month) %>% 
  tally(mean(value)) %>%
  arrange(month) %>% 
  mutate(color = ifelse((n > 0), "green","red"))

plotdata.files %>% 
  ggplot() + 
  geom_col(aes(x = doc_id, y = n, fill = color)) +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("green" = "green",
                               "red" = "red"),
                    labels = c("positive", "negative"),
                    name = "legend") +
  xlab("text files (April - December)") +
  ylab("value") +
  ggtitle("Positive and negative words across each newspaper article between April and December, 1904")

#### Natural Language Processing ####

## experimenting with spacyr

txt <- readLines(con = "txt_files/")

parsedtxt <- spacy_parse(raw.data$text)

colnames(parsedtxt)[4] <- "word"

sent.parsed.data <- parsedtxt %>% 
  inner_join(sent)
