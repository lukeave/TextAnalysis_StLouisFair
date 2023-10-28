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

#### experimenting with spacy_parse ####

#parse the text from the text data
parsedtxt <- spacy_parse(text.data, lemma = FALSE, additional_attributes = c("ent_iob_"))

#must assign "NORP" as entity type to every instance of "philippines" and its variations, as well as "visayans" and its variations in the data
parsedtxt <- parsedtxt %>% 
  mutate(entity = ifelse(grepl("philip", token) | grepl("filip", token) | grepl("visay", token), "NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("visaran", token), "NORP", entity))

#filter parsedtxt to words that are NORP or GPE only
entities <- parsedtxt %>% 
  filter(grepl('NORP', entity) | grepl('GPE', entity))
#don't worry about removing non-entities, typos, etc. from the entities data yet -- this can be more easily done from the count data frame

#count
entities.count <- entities %>%
  group_by(entity) %>% 
  count(token, sort = TRUE) %>% 
  as.data.frame() #this df allowed me to do a close reading of each entity and collapse them into a new set of place_names

#remove tokens "st", "louis" and "louisiana" as these will skew the data
entities.count <- entities.count[-c(1, 2, 15), ]
row.names(entities.count) <- 1:nrow(entities.count)
#remove token "states" - you don't want to double count "united states" when summarizing counts of "united" and "states" 
entities.count <- entities.count[-1,]
row.names(entities.count) <- 1:nrow(entities.count)
#will remove more erratic tokens later 

#part of me wonders if the spacy_extract_entity() function can help in distinguishing UK from US in the entities.count df 
## I decided not to rely on this function for the overall analysis because it does not recognize nearly as many instances of Philippines as an entity, for example.
test <- spacy_extract_entity(text.data)
#the output suggests that only one out of 149 recognized "united" entities refers to the United Kingdom in the corpus. So it looks like it is okay to assign the new place name of "United States" to every occurrence of "united".
rm(test)

#apply fct_collapse() to the new df to get proper set of place names 
## must count again after this step is complete

entities.count <- entities.count %>% 
  mutate(place_name = fct_collapse(token, "United States" = c("united", "american", "americans", "america"),
                                   "Alaska" = c("alaska", "alaskan"),
                                   "Russia" = c("russia", "russian"),
                                   "Missouri" = c("mixsourt", "missouri", "misseuri"),
                                   "Africa" = c("affican", "africa", "african"),
                                   "Brazil" = c("brazi", "brazilian", "brazil"),
                                   "Germany" = c("german", "germany", "germans"),
                                   "Washington DC" = c("washington", "wasnington", "wasa"),
                                   "New York" = "york",
                                   "Chicago" = "chicago",
                                   "France" = c("france", "french"),
                                   "China" = c("china", "chinese"),
                                   "Kansas" = c("kans", "kansas"),
                                   "Japan" = c("japan", "japanese"),
                                   "Paris" = "paris",
                                   "Texas" = c("thétexas", "texason", "texas"),
                                   "United Kingdom" = c("british", "britain", "england", "kingdom"),
                                   "Ireland" = c("irish", "irishamericans", "irishmans"),
                                   "California" = c("cali", "fornia", "california"),
                                   "Mexico" = c("mexico", "mexican", "mexicans"),
                                   "Ohio" = "ohio",
                                   "London" = "london",
                                   "Indiana" = "indiana",
                                   "Indianapolis" = c("indianapolis", "dianapolis"),
                                   "Canada" = c("canadian", "canada", "canadians"),
                                   "Toronto" = "toronto",
                                   "Kentucky" = "kentucky",
                                   "Illinois" = c("hniinoisans", "illinois"),
                                   "Italy" = c("italy", "italian", "italians"),
                                   "Boston" = "boston",
                                   "Colorado" = "colorado",
                                   "Philippines" = c("filipino", "filipinos", "fillpino"),
                                   "Oklahoma" = "oklahoma",
                                   "Austria-Hungary" = c("austrohungarian", "austrian", "austria", "hungary"),
                                   "Berlin" = "berlin",
                                   "San Francisco" = c("san", "francisco"),
                                   "New Orleans" = "orleans",
                                   "Belgium" = c("belgium", "belgian"),
                                   "Cuba" = c("cuban", "cuba"),
                                   "Virginia" = "virginia",
                                   "Netherlands" = c("dutch", "netherlands"),
                                   "Europe" = c("european", "pean"),
                                   "Philadelphia" = "philadelphia",
                                   "Jerusalem" = c("jerusalem", "jerwsalem"),
                                   "Montana" = "montana",
                                   "Havana" = "havana",
                                   "Utah" = "utah",
                                   "Pennsylvania" = "pennsylvania",
                                   "Syria" = c("syrian", "syria"),
                                   "Sweden" = c("sweden", "swedish"),
                                   "Arkansas" = c("ark", "arkansas"),
                                   "Cincinnati" = "cincinnati",
                                   "Holland" = "holland",
                                   "Minneapolis" = "minneapolis",
                                   "Pittsburg" = "pittsburg",
                                   "Rome" = "rome",
                                   "Springfield" = "springfield",
                                   "Arizona" = "arizona",
                                   "Denver" = "denver",
                                   "Georgia" = "georgia",
                                   "Iowa" = c("iowa", "towa"),
                                   "Louisville" = "louisville",
                                   "Maine" = "maine",
                                   "Maryland" = "maryland",
                                   "Nevada" = "nevada",
                                   "Spain" = c("spanish", "spain", "spanishamerican"),
                                   "Switzerland" = "switzerland",
                                   "Atlanta" = "atlanta",
                                   "Connecticut" = "connecticut",
                                   "Dallas" = "dallas",
                                   "Houston" = "houston",
                                   "Mississippi" = "mississippi",
                                   "Milwaukee" = "milwaukee",
                                   "Minnesota" = "minnesota",
                                   "Oregon" = "oregon",
                                   "Persia" = c("persia", "persian"),
                                   "Sacramento" = "sacramento",
                                   "Scotland" = c("scottish", "scotland"),
                                   "Vienna" = "vienna",
                                   "Alton" = "alton",
                                   "Los Angeles" = "angeles",
                                   "Argentine" = "argentine",
                                   "Asia" = c("asian", "asians"),
                                   "Baltimore" = "baltimore",
                                   "Birmingham" = "birmingham",
                                   "Bridgeton" = "bridgeton",
                                   "Cairo" = "cairo",
                                   "Denmark" = "danish",
                                   "New Hampshire" = "hampshire",
                                   "Idaho" = "idaho",
                                   "New Jersey" = "jersey",
                                   "Manila, Philippines" = "manila",
                                   "Muscogee (Creek) Nation" = "muskogee",
                                   "Nashville" = "nashville",
                                   "Nicaragua" = "nicaragua",
                                   "Norway" = c("norwegian", "norway"),
                                   "Patagonia" = "patagonian",
                                   "Portugal" = "portugal",
                                   "Visayas, Philippines" = c("visayan", "visayans", "visaran", "visayas", "visaydn")
  ))

#remove remaining rows that are clearly not entities
entities.count <- entities.count[-c(), ]

#NOT DONE YET
#count new place names
entities.count <- entities %>%
  count(place_names, sort = TRUE) %>% 
  as.data.frame()
