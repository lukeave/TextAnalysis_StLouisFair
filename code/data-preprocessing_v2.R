######## July 2024 Script ########

##### Overview #####
# 1. Load Libraries
# 2. Set Tessaract to English
# 3. Set local paths to article clippings
# 4. Perform OCR on each file found, then write out to txt files folder
# 5. Clear Variables (if desired)
# 6. Get list of created text files (set to Validated for testing/validation)
# 7. For each text file, strip words split across lines (examp-le -> example) and write file back out (uses update_text function)
# 8. Create dataframe of text file data, combine with metadata
# 9. Create raw.data dataframe from left join of metadata and txt.files
# 10. Create text.data dataframe from raw.data and pre-process the text to set all letters to lowercase, remove numbers, and remove punctuation
# 11. Store data if desired
# 12. Tokenize text.data 
# 13. Remove custom and common stopwords, and numbers if any remain
# 14. Load qdap dictionaries
# 15. Remove first 106 rows of DICTIONARY dictionary, which contained garbage text
# 16. Create remove_trailing_s function to depluralize words
# 17. Create is_defined function to check if normal or depluralized word are in either GradyAugmented or DICTIONARY dictionary
# 18. Populate "exists_in_grady" or "exists_in_dict" columns in token.data to reflect presence in dictionary
# 19. Convert GradyAugmented to dataframe
# 20. Update 'exists' columns after comparing each word to both dictionaries
# 21. Create mistranslations dataframe
# 22. If word does not exist in both dictionaries, print filename, word, and the words occurring before and after to the mistranslations dataframe to be used for manual validation

#### Load libraries ####

library(tesseract)
library(readtext)
library(dplyr)
library(tidytext)
library(qdapDictionaries)
library(stringr)
library(readr)

#### Create Original Text Files ####

# establish tesseract language parameter
english <- tesseract("eng")
# list all jpg files
jpgs <- list.files("Newspaper_articles/")
# store relative path of jpg files
path <- paste("Newspaper_articles/")
# store relative path for output txt files
newdir <- paste("txt_files/")
# loop and OCR
for (i in 1:length(jpgs)) {
  filename <- paste(jpgs[i]) # catch the file's name
  fullpath <- paste0(path, filename) # store the file's relative path
  result <- tesseract::ocr(fullpath) # use the file's relative path to OCR
  #confidence_result <- tesseract::ocr_data(fullpath) # get confidence levels
  print(filename) # show me which file is being processed
  write_file(result, file = paste0(newdir, filename, ".txt")) # write output out as txt file in new txt_files directory
  #write_csv(confidence_result, file = paste0(confdir, filename, "_confidence.csv"))  # write csv file for confidence level and bounding box of words
} # Decided not to use tesseract's default confidence levels as they were misleading without model training.

# remove OCR loop variables
rm(i)
rm(newdir)
rm(jpgs)
rm(confdir)
rm(path)
rm(confidence_result)
rm(result)

# make table with all confidence levels and bbox parameters provided by Tesseract
setwd("new data/confidence/") # change working directory temporarily
files <- list.files(pattern = "*.csv") # list all csv files as a list
confidence_data <- files %>% 
  lapply(read.csv) %>% # apply read.csv() to each element on the list
  bind_rows() # combine all tables in a single df
setwd("../../../Lovett_Avelar_OCR/") # reset working directory to project directory

# This has proven to be inefficient. Tesseract's confidence levels do not mean anything if the model is not trained on accurate data first.

# function to correct words split across lines in text file
update_text <- function(text) {
  # remove -/n and -
  updated_text <- str_replace_all(text,"- ", "")
  updated_text <- str_replace_all(updated_text,"-\n", "")
  return(updated_text)
}

# build a validation text file set to be corrected manually
setwd("txt_files-Validated")
val.files <- list.files(pattern = "*.txt")

# initial pass to correct words split across lines then write back to original file in txt_files-Validation directory
for (i in 1:length(val.files)) {
  filename <- paste(val.files[i]) # catch the file's name
  text <- readtext(filename) #read in file content
  updated_text <- update_text(text) # pass function to correct split words
  print(filename) # show me which file is being processed
  write_file(updated_text, file = filename) # write output out as txt file in new txt_files directory
  
} 

#### Build Data Frames ####

# create data frame of text data with doc_id and full text
txt.files <- readtext(file.path("txt_files-Validated/"))
txt.files <- txt.files %>% 
  arrange(doc_id)
txt.files <- data.frame(gsub("\\.jpg\\.txt", ".txt", txt.files$doc_id),txt.files$text)

# fix txt.files column names (not sure why they are being modified in the first place)
txt.files <- txt.files %>%
  rename(doc_id = gsub.....jpg...txt.....txt...txt.files.doc_id.) %>%
  rename(text = txt.files.text)

# read in metadata associated with each file
metadata <- read.csv("data/metadata.csv")
metadata$doc_id <- paste0(metadata$doc_id, metadata$file_format) # prepare for join
metadata <- metadata[,-2]
metadata <- metadata %>% 
  arrange(doc_id)

# create raw.data data frame from joining txt.files and metadata
raw.data <- metadata %>% 
  left_join(txt.files, by = "doc_id") #this df should have a column with the entire text for each file

# build a text.data data frame
## it should have a cleaner version of the entire text for each file per observation
### strip out punctuation and uppercase letters form raw data
text.data <- raw.data
text.data$text <- raw.data$text %>%
  str_to_lower() %>%
  str_replace_all("[:punct:]", "") %>% 
  #str_replace_all("- ", "") %>% #(Already completed above using update_text function) Because a lot of hyphenated words that were split across different lines became separated by "- " in the output files, this line strips out the combo (hyphen + space) from the corpus to try and catch those previously hyphenated words as a single token. 
  str_remove_all("~") %>% 
  str_remove_all("|") %>% 
  str_replace_all("[^[:alnum:]]", " ")

# store text data
write.csv(text.data, file = "data/text_data.csv")

# create first iteration of the tokenized data
token.data <- text.data %>%
  unnest_tokens(word, text)

# customize stop words list based on a first educated guess of which words could skew the analysis
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

# to be able to catch more stopwords, typos, and OCR errors that are specific to this data, run a preliminary word count followed by close reading to determine a second list of stop words
# preliminary count of words
word.count <- token.data %>%
  anti_join(stop_words_custom) %>% #here, removing the words already expected to skew the data helps to find different words
  count(word, sort = TRUE) %>% 
  as.data.frame()

# remove more irrelevant words
other.stop.words <- c("day", "building", "company", "miss","yesterday","exhibits")
#other.stop.words <- c("fe", "tions", "ey", "ap", "ea", "ee", "day", "building", "company", "ae", "miss", "oe", "yesterday", "ing", "se", "es", "de", "te", "eee", "tion", "en", "er", "ss", "ts", "lols", "exhibits", "con", "al", "tn", "pe", "ne", "ad", "ed", "ar", "ef", "gov", "im", "ge", "os", "eae", "ay", "ot", "aa", "dis", "mo", "tt", "dr", "pa", "ly", "ag", "fs") - Decided to keep these errors in order to try and manually catch actual words using the data validation method through dictionaries below.
other.stop.words <- other.stop.words %>% 
  as.data.frame()
colnames(other.stop.words)[1] <- "word"

# remove original stop words, new stop words, and numeric values from token.data
token.data <- token.data %>% 
  anti_join(other.stop.words) %>% 
  anti_join(stop_words_custom) %>% 
  filter(!grepl("[[:digit:]]", word))

# count again
word.count <- token.data %>%
  count(word, sort = TRUE) %>% 
  as.data.frame()

#### Data Validation ####

# explore the JPG files to brainstorm potential validation methods
## JPGs are widely different in dimensions, size and quality. One way to tackle this is to split them into different directories based on the file size.

# define sorting parameters
source_dir <- "txt_files" # source directory with all text files
dest_small <- "Newspaper_articles/small_files" # small files destination
dest_medium <- "Newspaper_articles/medium_files" # medium files destination
dest_large <- "Newspaper_articles/large_files" # large files destination
min_small <- 0  # small word count range
max_small <- 300 
min_medium <- 301 # medium word count range
max_medium <- 900 
min_large <- 901 # large word count range
max_large <- 5000 

# function to move file if within small word count range
move_file_if_small <- function(file_path) {
  file_name <- basename(file_path)
  dest_path <- file.path(dest_small, file_name)
  file.rename(file_path, dest_path)
}

# function to move file if within medium count range
move_file_if_medium <- function(file_path) {
  file_name <- basename(file_path)
  dest_path <- file.path(dest_medium, file_name)
  file.rename(file_path, dest_path)
}

# function to move fil if within large count range
move_file_if_large <- function(file_path) {
  file_name <- basename(file_path)
  dest_path <- file.path(dest_large, file_name)
  file.rename(file_path, dest_path)
}

# get word count per each file
file_word_count <- token.data %>%
  count(doc_id, sort = TRUE) %>% 
  as.data.frame()

# remove ".txt" from file names
file_word_count <- file_word_count %>%
  mutate(doc_id = gsub(".txt", "", doc_id))
head(file_word_count)

# loop and sort JPG files
## make sure doc_id in word.count has the same file name of JPG files (no ".txt")
for (i in 1:nrow(file_word_count)) {
  if (file_word_count$n[i] >= min_large) {
    move_file_if_large(paste0("Newspaper_articles/", file_word_count$doc_id[i])) # move large ones first
  }
  else if (file_word_count$n[i] >= min_medium) {
    move_file_if_medium(paste0("Newspaper_articles/", file_word_count$doc_id[i])) # move medium ones
  }
  else if (file_word_count$n[i] >= min_small) {
    move_file_if_small(paste0("Newspaper_articles/", file_word_count$doc_id[i])) # move small ones
  }
}

# Sorting the JPGs helped identify common OCR issues in files of similar sizes. But training tesseract to have different OCR parameters for each file size would take a long time, and this level of automation might be more useful in the future when acquiring new data.

# Instead, the validation method below uses two different dictionaries to compare with the original corpus and determine whether the word resulting from the OCR exists or not.

# load the GradyAugmented dataset from qdapDictionaries
data("GradyAugmented")
# load the DICTIONARY dataset from qdapDictionaries
data("DICTIONARY")
# remove trash entries from beginning of DICTIONARY
DICTIONARY$rowID <- seq_len(nrow(DICTIONARY))
DICTIONARY <- DICTIONARY[-(1:106), ]

# function to depluralize potentially plural words, which DICTIONARY does not contain (singular only)
remove_trailing_s <- function(word) {
  # Check if the word ends with 's'
  if (substr(word, nchar(word), nchar(word)) == "s") {
    # Remove the trailing 's'
    word <- substr(word, 1, nchar(word) - 1)
  }
  return(word)
}

# generic function to test original and depluralized word against whichever dictionary is passed in
is_defined <- function(row, dictionary) {
  word_no_s <- remove_trailing_s(row$word)
  defined <- "False"
  if (row$word %in% dictionary$word){
    defined <- "True"
    return(defined)
  }
  if (word_no_s %in% dictionary$word){
    defined <- "True"
    return(defined)
  }
  return(defined)
}

# adding column to indicate if it exists in GradyAugmented dictionary
token.data <- token.data %>%
  mutate(exists_in_grady = "False")

# adding a column to indicate if it exists in DICTIONARY dictionary
token.data <- token.data %>%
  mutate(exists_in_dict = "False")

# transform GradyAugmented variable to a data frame and rename default column to 'word' to align with DICTIONARY
GradyAugmented <- GradyAugmented %>%
  as.data.frame()
GradyAugmented <- GradyAugmented %>%
  rename(word = '.')

# populate token.data columns with if the word exists in the dictionaries
for (i in 1:nrow(token.data)) {
  token.data[i,'exists_in_dict'] <- is_defined(token.data[i,], DICTIONARY)
  token.data[i,'exists_in_grady'] <- is_defined(token.data[i,], GradyAugmented)
  print(i)
}

# create mistranslations dataframe
column_names <- c("article", "word", "wordbefore", "wordafter")
# create an empty data frame with the specified columns
mistranslations <- data.frame(matrix(ncol = length(column_names), nrow = 0))
# set the column names
colnames(mistranslations) <- column_names

# populate mistranslations data frame with outcome of testing both dictionaries with normal and depluralized word
for (i in 1:nrow(token.data)) {
  narticle <- token.data[i,'doc_id'] # txt file name
  nword <- token.data[i,'word'] # mistranslated word
  nwordbefore <- "NA" # word that came before (potential word-part)
  nwordafter <- "NA" # word that came after (potential word-part)
  # this section won't try to print the previous or following word if at the beginning or end of the word list
  if (i > 1) { 
    nwordbefore <- token.data[i-1,'word']
  }
  if (i < nrow(token.data)) {
    nwordafter <- token.data[i+1,'word']
  }
  # populate Mistranslations with undefined words
  if ((token.data[i,'exists_in_dict'] == "False") &&  (token.data[i,'exists_in_grady'] == "False")) {
    new_row <- data.frame(article = narticle, word = nword, wordbefore = nwordbefore, wordafter = nwordafter)
    mistranslations <- rbind(mistranslations, new_row)
  }
}

#### Manual Validation Allowed Methods ####
## Stripping phantom punctuation
## Word correction
## word insertion if missed
## word reconnection if split across lines
