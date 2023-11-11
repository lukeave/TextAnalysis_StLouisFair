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
getwd()
#### Tesseract ####

#establish tesseract language parameters
english <- tesseract("eng")

#list all jpg files in a new variable 
jpgs <- list.files("../Newspaper_articles/")
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

# before the following code was applied to the data, I went in and stripped out every line break (\n) in each text file resulted from the OCR. I did this by running the following command on the terminal:

#for f in *.txt
##do
#### tr "\n" " " < "$f > $(basename "$f" .txt)-out.txt

#this is catching every instance of line break in each file and replacing it with a space.
#with the new 461 output files, I renamed them to replace the original files in the txt_files directory, and moved on with the preprocessing in RStudio.

# building a corpus from the text files
txt.files <- readtext(file.path("txt_files/"))
txt.files <- txt.files %>% 
  arrange(doc_id)

metadata <- read.csv("data/metadata.csv")
metadata$doc_id <- paste0(metadata$doc_id, metadata$file_format)
metadata <- metadata[,-2]
metadata <- metadata %>% 
  arrange(doc_id)

raw.data <- metadata %>% 
  left_join(txt.files, by = "doc_id") #this df should have a column with the entire text for each file

#store raw data prior to any further cleaning
#it should have the original OCR'd text in the text column prior to any data processing
write.csv(raw.data, file = "data/raw_data.csv")

## building a text.data dataframe
#it should have a cleaner version of the entire text for each file per observation
# strip out punctuation and uppercase letters form raw data
text.data <- raw.data
text.data$text <- raw.data$text %>%
  str_to_lower() %>%
  str_replace_all("[:punct:]", "") %>% 
  str_replace_all("- ", "") %>% #Because a lot of hyphenated words that were split across different lines became separated by "- " in the output files, this line strips out the combo (hyphen + space) from the corpus to try and catch those previously hyphenated words as a single token. 
  str_remove_all("~") %>% 
  str_remove_all("|") %>% 
  str_replace_all("[^[:alnum:]]", " ")

#store text data
write.csv(text.data, file = "data/text_data.csv")

## tokenization
#for the tokenized data, stop words should be removed.

#get first iteration of the tokenized data
token.data <- text.data %>%
  unnest_tokens(word, text)

#customize stop words list based on a first educated guess of which words could skew the analysis
stopwords <- stop_words
stop_words_custom <- stop_words %>% 
  add_row(word="fair", lexicon="NA") %>% 
  add_row(word="world", lexicon="NA") %>% 
  add_row(word="world's ", lexicon="NA") %>% 
  add_row(word="world's", lexicon="NA") %>% 
  add_row(word="worlds", lexicon="NA") %>% 
  add_row(word="exhibit", lexicon="NA") %>% 
  add_row(word="exposition", lexicon="NA") %>% 
  add_row(word="louisiana", lexicon="NA") %>% 
  add_row(word="purchase", lexicon="NA") %>% 
  add_row(word="st", lexicon="NA") %>% 
  add_row(word="louis", lexicon="NA")
rm(stopwords)
#to be able to catch more stopwords, typos, and OCR errors that are specific to this data, run a preliminary word count followed by close reading to determine a second list of stop words
#preliminary count of words
word.count <- token.data %>%
  anti_join(stop_words_custom) %>% #here, removing the words already expected to skew the data helps to find different words
  count(word, sort = TRUE) %>% 
  as.data.frame()

#remove more irrelevant words
#this will require a close reading of the word.count dataframe

other.stop.words <- c("fe", "tions", "ey", "ap", "ea", "ee", "day", "building", "company", "ae", "miss", "oe", "yesterday", "ing", "se", "es", "de", "te", "eee", "tion", "en", "er", "ss", "ts", "lols", "exhibits", "con", "al", "tn", "pe", "ne", "ad", "ed", "ar", "ef", "gov", "im", "ge", "os", "eae", "ay", "ot", "aa", "dis", "mo", "tt", "dr", "pa", "ly", "ag", "fs")
other.stop.words <- other.stop.words %>% 
  as.data.frame()

colnames(other.stop.words)[1] <- "word"

#remove original stop words, new stop words, and numeric values
token.data <- token.data %>% 
  anti_join(other.stop.words) %>% 
  anti_join(stop_words_custom) %>% 
  filter(!grepl("[[:digit:]]", word))

#count again
word.count <- token.data %>%
  count(word, sort = TRUE) %>% 
  as.data.frame()

#store tokenized data
write.csv(token.data, file = "data/tokenized_data.csv")