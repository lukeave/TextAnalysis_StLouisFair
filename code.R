library(tidyverse)
library(tesseract)

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
#THIS WORKED!!!!!!! 

#example of loop
#presidents <- c("George Washington", "John Adams", "Thomas Jefferson")
#for (p in 1:length(presidents)) {
 # print(presidents[p])
#}
