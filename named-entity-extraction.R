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

#### Named Entity Extraction ####

#parse the text from the text data
parsedtxt <- spacy_parse(text.data, lemma = FALSE, additional_attributes = c("ent_iob_"))

parsedtxt <- parsedtxt %>% 
  rowid_to_column(var = "rowid")

#assign "NORP" as entity type to every instance of "philippines", "visayans", "cocopas", "sioux" and its variations in the data, as well as other known Indigenous groups
parsedtxt <- parsedtxt %>% 
  mutate(entity = ifelse(grepl("philip", token) | grepl("filip", token), "NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("visaran", token) | grepl("visay", token), "NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("cocop", token), "NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("siou", token), "NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("troquois", token), "NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("moqui", token) | grepl("moki", token),"NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("zuni", token), "NORP", entity))

#filter parsedtxt to create entities df
entities <- parsedtxt %>% 
  filter(grepl('NORP', entity) | grepl('GPE', entity))

#preliminary count -- this will be done again later
entities.count <- entities %>%
  group_by(entity) %>% 
  count(token, sort = TRUE) %>% 
  as.data.frame() #this df allowed me to perform close reading of each entity and note which ones could be altered in the entities df before moving forward

#### Data Annotation #### 

## Annotate the entities df prior to cleaning the data
#DO NOT remove any rows before this step is complete
entities <- entities %>% 
  mutate(annotation = "NA")

#catch accurate place references for all occurrences of the token "new"
entities <- entities %>% 
  mutate(annotation = ifelse(rowid %in% c(557, 13634, 24222, 26437, 26979, 28023, 28102, 28428, 28952, 30053, 31619, 34302, 47318, 47345, 48359, 48805, 49207, 53245, 61837, 64180, 67350, 79796, 85018, 85257, 86856, 86876, 93792, 93810, 95595, 98671, 105491, 108190, 108231, 108258, 108291, 108305, 110895, 110910, 111255, 114148, 122845, 132476, 144678, 158226, 163239, 163417, 163539, 177183, 178592, 195508, 202407, 206914, 207040, 207075, 207262, 207290, 207306), "NY", #New York
                        ifelse(rowid %in% c(30059, 48265, 49823, 50725, 82773, 157388, 158330, 172295, 176918), "NOLA", #New Orleans
                               ifelse(rowid %in% c(33021, 33046, 33081, 48382, 82845, 167455), "NJ", #New Jersey
                                      ifelse(rowid %in% c(78818, 85243), "NH", #New Hampshire
                                             ifelse(rowid == 84193, "NM", #New Mexico
                                                    annotation))))))

#catch accurate place references for all occurrences of the token "mexico"
entities <- entities %>% 
  mutate(annotation = ifelse(rowid %in% c(24110, 48833, 49138, 51518, 51535, 51552, 74321, 84194, 111123, 119511, 139826, 176480, 176502, 181724, 182612, 192746, 192968, 193000, 193534), "MEX", #Mexico
                             ifelse(rowid %in% c(2479, 34621), "CCP", #Cocopah
                                    ifelse(rowid %in% c(50680, 50933), "GM", #Gulf of Mexico
                                           annotation))))

#catch accurate place references for all occurrences of the token "india*"
entities <- entities %>% 
  mutate(annotation = ifelse(rowid %in% c(2493, 2517, 2665, 10491, 13453, 15079, 15093, 26254, 28119, 28172, 28291, 28309, 28321, 31123, 36973, 39803, 50324, 61166, 66165, 69596, 69628, 69893, 111527, 111556, 111565, 111608, 113060, 115177, 123943, 123952, 124042, 124063, 124071, 124102, 124259, 130019, 145828, 145897, 145914,147155, 147255, 147270, 147290, 147362, 147438, 147945, 148055, 148065, 148099, 148201, 148245, 148333, 165063, 165126, 167692, 171586, 171625, 171637, 171653, 171754, 171776, 171924, 172079, 172107, 172134, 174504, 174516, 176777, 176797, 177816, 177839, 177862, 177990, 178150, 178207, 179638, 191066, 191106, 191130, 191133, 191181, 194610, 194652, 203211, 203232, 203342), "NTV", #General meaning of Native
                             ifelse(rowid %in% c(10453, 10476, 10540), "AZ", #Pima, Americopa, and Papago
                                    ifelse(rowid %in% c(2464, 177292, 177315, 177377, 186865), "CCP", #Cocopah
                                           ifelse(rowid %in% c(34629, 169806), "KWK", #Kwakiutl
                                                  ifelse(rowid %in% c(17049, 84677), "MQ", #Moqui
                                                         ifelse(rowid %in% c(2698, 48392), "SX",#Sioux
                                                               ifelse(rowid %in% c(18085, 51505, 71607, 104051, 130236, 130736, 131699, 166770, 166884, 178598, 179657, 180198, 180210, 186535, 186566, 197763, 197895, 201477), "IN", #Indiana
                                                                      ifelse(rowid %in% c(147343), "PB", #Pueblo
                                                                             ifelse(rowid %in% c(164048), "IMD", #India
                                                                  annotation))))))))))


#mutate the original token column in the entities df
entities <- entities %>% 
  mutate(token = ifelse(annotation == "NY", "new_york",
                        ifelse(annotation == "NH", "new_hampshire",
                               ifelse(annotation == "NM", "new_mexico",
                                      ifelse(annotation == "NJ", "new_jersey",
                                             ifelse(annotation == "NOLA", "new_orleans",
                                                    ifelse(annotation == "MEX", "mexico",
                                                           ifelse(annotation == "CCP", "cocopah_reservation",
                                                                  ifelse(annotation == "GM", "gulf_mexico",
                                                                         ifelse(annotation == "NTV", "native",
                                                                         ifelse(annotation == "AZ", "pima_americopa_papago",
                                                                                ifelse(annotation == "KWK", "kwakiutl",
                                                                                       ifelse(annotation == "MQ", "moqui",
                                                                                              ifelse(annotation == "IN", "indiana",
                                                                                                     ifelse(annotation == "SX", "sioux",
                                                                                                            ifelse(annotation == "PB", "pueblo",
                                                                                                            ifelse(annotation == "IMD", "india",
                                                                                                                   token))))))))))))))))) 

#### Data Cleaning ####

#remove rows with token "india*" that should not be counted as entities
entities <- entities %>% 
  filter(!rowid == 55903) # "indian summer"

# second count -- this time, without grouping by entity type
entities.count <- entities %>%
  count(token, sort = TRUE) %>% 
  as.data.frame() #use this df to retrieve a list of terms that will be used in the code below.

## when the list is complete, apply fct_collapse on the entities df to get proper place_names (NOT on the entities.count df)
## must count again after this step is complete

entities <- entities %>% 
  mutate(place_name = fct_collapse(token, "United States" = c("united", "american", "americans", "america"),
                                   "Alaska" = c("alaska", "alaskan"),
                                   "Russia" = c("russia", "russian"),
                                   "Missouri" = c("mixsourt", "missouri", "misseuri"),
                                   "Africa" = c("affican", "africa", "african"),
                                   "Brazil" = c("brazi", "brazilian", "brazil"),
                                   "Germany" = c("german", "germany", "germans"),
                                   "Washington DC" = c("washington", "wasnington", "wasa"),
                                   "New York" = c("york", "new_york"),
                                   "Chicago" = "chicago",
                                   "France" = c("france", "french"),
                                   "China" = c("china", "chinese", "cina", "ckinese"),
                                   "Kansas" = c("kans", "kansas"),
                                   "Japan" = c("japan", "japanese"),
                                   "Paris" = "paris",
                                   "Texas" = c("thétexas", "texason", "texas"),
                                   "United Kingdom" = c("british", "britain", "england", "kingdom", "britannia"),
                                   "Ireland" = c("irish", "irishamericans", "irishmans"),
                                   "California" = c("cali", "fornia", "california"),
                                   "Mexico" = c("mexico", "mexican", "mexicans", "merican"),
                                   "Ohio" = "ohio",
                                   "London" = "london",
                                   "Indiana" = "indiana",
                                   "Indianapolis" = "dianapolis",
                                   "Canada" = c("canadian", "canada", "canadians"),
                                   "Toronto" = "toronto",
                                   "Kentucky" = "kentucky",
                                   "Illinois" = c("hniinoisans", "illinois"),
                                   "Italy" = c("italy", "italian", "italians"),
                                   "Boston" = "boston",
                                   "Colorado" = "colorado",
                                   "Philippines" = c("filipino", "filipinos", "fillpino","philippine","philippines","philip", "philipines","philipipne", "philippiies", "philipping","philippne", "filipines", "filipind", "filipinas", "filipine", "filipiso", "filipiz", "filiplines", "visayan", "visayans", "visaran", "visayas", "visaydn", "manila"),
                                   "Oklahoma" = "oklahoma",
                                   "Austria-Hungary" = c("austrohungarian", "austrian", "austria", "hungary"),
                                   "Berlin" = "berlin",
                                   "San Francisco" = c("san", "francisco"),
                                   "New Orleans" = c("orleans", "new_orleans"),
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
                                   "New Hampshire" = c("hampshire","new_hampshire"),
                                   "Idaho" = "idaho",
                                   "New Jersey" = c("jersey", "new_jersey"),
                                   "Muscogee (Creek) Nation" = "muskogee",
                                   "Nashville" = "nashville",
                                   "Nicaragua" = "nicaragua",
                                   "Norway" = c("norwegian", "norway"),
                                   "Patagonia" = "patagonian",
                                   "Portugal" = "portugal",
                                   "Inner Mongolia" = "manchuria",
                                   "Zimbabwe" = "rhodesia",
                                   "Alabama" = "alabama",
                                   "Australia" = "australia",
                                   "Amsterdam" = "armeterdam",
                                   "Bulgaria"  = "bulgaria",
                                   "Colombia" = "colombia",
                                   "Haiti" = "haiti",
                                   "India" = "india",
                                   "Iroquois" = "troquois",
                                   "Sioux" = c("sioux", "sious"),
                                   "Cocopah" = c("cocopah_reservation", "cocopas", "cocopa"),
                                   "Moqui" = c("mokis", "moki", "moqui"),
                                   "Salt River Pima–Maricopa Indian Community" = "pima_americopa_papago",
                                   "Kwakiutl" = "kwakiutl",
                                   "Richmond, KY" = "richmond",
                                   "Venice, Italy" = "venice",
                                   "Vermont" = "vermont",
                                   "Wisconsin" = "wisconsin"
  ))

#based on the examination of the entities.count df, list tokens that must be removed from the entities df
##if a token has less than 2 counts in the entities.count df, no need to include it in the false.tokens list
###make sure to include"states" to avoid double counting "united states" when summarizing counts of "united" and "states"
false.tokens <- c("st", "louis", "louisiana", "louisi", "louislana", "the", "city", " ", "  ", "states", "native", "mans", "delmar", "english", "east", "la", "west", "jefferson", "plaza", "republican", "al", "columbian", "dem", "dian", "sa", "smoking", "south", "womans", "vt", "christian", "county", "day", "democrat", "jand", "los", "louls", "loutsiana", "mo", "of", "panamerican", "us", "and", "bt", "coast", "democrats", "fu", "fort", "guards", "gulf_mexico", "kas", "medora", "nese", "north", "officiais", "reardon", "republicans", "sea", "sean", "smith", "sou", "thorpe", "to", "toledo", "willis", "   ", "12", "yt") 

#remove false tokens
entities <- entities %>% 
  filter (!token %in% false.tokens)

#### Getting Place Names ####

# third count -- this time, counting the newly assigned place names in the entities.count df
placenames <- entities %>%
  group_by(place_name) %>% 
  summarize(count = n()) %>% 
  as.data.frame()

#filter placenames df to remove statistically irrelevant counts
placenames <- placenames %>% 
  filter(count > 1) %>% 
  arrange(desc(count))

#create lists of place names by scale
cities <- c()
states <- c()
countries <- c()
continents <- c()
native_nations <- c()

#create a new variable in the placenames df that distinguishes tokens by scale 
#(i.e., city, state, country, continent, native_nation)
placenames <- placenames %>% 
  mutate(scale = ifelse(token %in% cities, "city",
                        ifelse(token %in% states, "state",
                               ifelse(token %in% countries, "country",
                                      ifelse(token %in% continents, "continents",
                                             ifelse(token %in% native_nations, "native_nation",
                                                    "NA"))))))

#### Geocoding ####


