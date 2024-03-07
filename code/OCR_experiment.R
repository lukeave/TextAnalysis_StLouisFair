library(tidyverse)
library(tesseract)
library(tidytext) 
library(magick)
library(dplyr)

#before feeding jpg files into tesseract, try and fix images to improve OCR accuracy
text_1 <- tesseract::ocr("../OCR experiment files/SLGD_1904_05_28_P3_002.jpg")

cat(text_1) #results without preprocessing image

input <- image_read("../OCR experiment files/SLGD_1904_05_28_P3_002.jpg")

text <- input %>%
  image_scale("3000") %>% 
  image_convert(type = 'Grayscale') %>%
  image_deskew(threshold = 40) %>% 
  image_contrast(sharpen = 15) %>% 
  image_modulate(brightness = 100) %>% 
  image_write(format = 'png') %>%
  tesseract::ocr() 

cat(text) #results after preprocessing image


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
