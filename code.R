library(tidyverse)
library(tesseract)
library(tidytext) 
library(readtext)
library(widyr)
library(SnowballC)

#establish tesseract language parameters
english <- tesseract("eng")
#list set of test files in a new variable 
testjpgs <- list.files("testocr_folder/")
#store relative path of test files in a new variable
path <- paste("testocr_folder/")
#store relative path for output txt files in a new variable
newdir <- paste("txt_files/")
#loop
for (i in 1:length(testjpgs)) {
  filename <- paste(testjpgs[i]) #catch the file's name
  fullpath <- paste0(path, filename) #store the file's relative path
  result <- tesseract::ocr(fullpath) #use the file's relative path to OCR
  print(result) #show me what's going on
  write.table(result, file = paste0(newdir, filename, ".txt"), sep = "\t") #write a new txt file in new txt_files directory
} 
#THIS WORKED!

#apply loop structure to actual JPG files

#establish tesseract language parameters
english <- tesseract("eng")
#list set of jpg files in a new variable 
jpgs <- list.files("Newspaper_articles/")
#store relative path of test files in a new variable
path <- paste("Newspaper_articles/")
#store relative path for output txt files in a new variable
newdir <- paste("txt_files/")
#loop
for (i in 1:length(jpgs)) {
  filename <- paste(jpgs[i]) #catch the file's name
  fullpath <- paste0(path, filename) #store the file's relative path
  result <- tesseract::ocr(fullpath) #use the file's relative path to OCR
  print(filename) #show me what's going on
  write_file(result, file = paste0(newdir, filename, ".txt")) #write a new txt file in new txt_files directory
} 

#### Data Preprocessing ####

# building a corpus

txt.files <- readtext(file.path("txt_files/"))
txt.files <- txt.files %>% 
  arrange(doc_id)

metadata <- read.csv("TextAnalysis_StLouisFair/metadata.csv")
metadata$doc_id <- paste0(metadata$doc_id, metadata$file_format)
metadata <- metadata[,-2]
metadata <- metadata %>% 
  arrange(doc_id)

data <- metadata %>% 
  left_join(txt.files, by = "doc_id")

#write.csv(data, file = "TextAnalysis_StLouisFair/data.csv")

stopwords <- stop_words

## tokenization

#customize stop words list
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
data <- data %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words_custom)

#raw count of words
word.count <- data %>%
  count(word, sort = TRUE) %>% 
  as.data.frame()

#random occurrence test (exploratory)
count.future.past <- data %>%
  filter(word %in% c("future", "past")) %>%
  group_by(month, word) %>% 
  summarise(count = n())

#plot results (exploratory)
count.future.past %>% 
  ggplot(aes(month, count, fill = word)) +
  geom_bar(position = "dodge", stat = "identity")


