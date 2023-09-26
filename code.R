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

raw.data <- metadata %>% 
  left_join(txt.files, by = "doc_id")

#write.csv(data, file = "TextAnalysis_StLouisFair/raw_data.csv")

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

#raw count of words
word.count <- data %>%
  count(word, sort = TRUE) %>% 
  as.data.frame()

#random occurrence test (exploratory)
count.future.past <- data %>%
  filter(word %in% c("future", "past")) %>%
  group_by(month, word) %>% 
  summarise(count = n())

#turn each month into a character for plotting (exploratory)
count.future.past$month[1] <- "April"
count.future.past$month[2] <- "April"
count.future.past$month[3] <- "May"
count.future.past$month[4] <- "May"
count.future.past$month[5] <- "June"
count.future.past$month[6] <- "June"
count.future.past$month[7] <- "July"
count.future.past$month[8] <- "July"
count.future.past$month[9] <- "August"
count.future.past$month[10] <- "August"
count.future.past$month[11] <- "September"
count.future.past$month[12] <- "October"
count.future.past$month[13] <- "October"
count.future.past$month[14] <- "November"
count.future.past$month[15] <- "November"
count.future.past$month[16] <- "December"
count.future.past$month[17] <- "December"

#turn each month into a factor for plotting (exploratory)
count.future.past$month <- 
  factor(count.future.past$month, levels=c("April", "May", "June", "July", "August", "September", "October", "November", "December"))

#plot results (exploratory)
count.future.past %>% 
  ggplot(aes(month, count, fill = word)) +
  geom_bar(position = "dodge", stat = "identity")


