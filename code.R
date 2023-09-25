library(tidyverse)
library(tesseract)

#establish tesseract language parameters
#english <- tesseract("eng")
#list set of test files in a new variable 
#testjpgs <- list.files("testocr_folder/")
#store relative path of test files in a new variable
#path <- paste("testocr_folder/")
#store relative path for output txt files in a new variable
#newdir <- paste("txt_files/")
#loop
#for (i in 1:length(testjpgs)) {
#  filename <- paste(testjpgs[i]) #catch the file's name
#  fullpath <- paste0(path, filename) #store the file's relative path
#  result <- tesseract::ocr(fullpath) #use the file's relative path to OCR
#  print(result) #show me what's going on
#  write.table(result, file = paste0(newdir, filename, ".txt"), sep = "\t") #write a new txt file in new txt_files directory
#} 
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


