library(tidyverse)
library(tidytext) 
library(readtext)
library(spacyr)
library(ggmap)
library(maps)
library(tidygeocoder)
library(stringr)
library(dplyr)
library(tidyr)

#### Named Entity Extraction ####

#parse the text from the text data
text.data <- read.csv("data/text_data.csv")
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
  mutate(entity = ifelse(grepl("zuni", token), "NORP", entity)) %>% 
  mutate(entity = ifelse(grepl("igorr", token), "NORP", entity))
#assign "NORP" as entity type to every instance of "america*" and its variations in the data 
#the actual entities data will be cleaned later, and tokens mistakenly classified as entities will be removed
parsedtxt <- parsedtxt %>% 
  mutate(entity = ifelse(grepl("merica", token) | grepl("ameri", token) | grepl("brican", token), "NORP", entity))

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
  mutate(annotation = ifelse(rowid %in% c(557, 13634, 24191, 26406, 26948, 27992, 28071, 28397, 28952, 30022, 31588, 34271, 47287, 47314, 48328, 48774, 49176, 53214, 61806, 64149, 67319, 79765, 84987, 85226, 86825, 86845, 93761, 93779, 95564, 98640, 105460, 108159, 108200, 108227, 108260, 108274, 110864, 110879, 111224, 114117, 122814, 132445, 144647, 158195, 163208, 163386, 163508, 177152, 178561, 195477, 202376, 206883, 207009, 207044, 207231, 207259, 207275), "NY", #New York
                        ifelse(rowid %in% c(30028, 48234, 49792, 50694, 82742, 157357, 158299, 172264, 176887), "NOLA", #New Orleans
                               ifelse(rowid %in% c(32990, 33015, 33050, 48351, 82814, 167424), "NJ", #New Jersey
                                      ifelse(rowid %in% c(78787, 85212), "NH", #New Hampshire
                                             ifelse(rowid == 84162, "NM", #New Mexico
                                                    annotation))))))

#catch accurate place references for all occurrences of the token "mexico"
entities <- entities %>% 
  mutate(annotation = ifelse(rowid %in% c(24079, 48802, 49107, 51487, 51504, 51521, 74290, 84163, 111092, 119480, 139795, 176449, 176471, 181693, 182581, 192715, 192937, 192969, 193503), "MEX", #Mexico
                             ifelse(rowid %in% c(2479, 34590), "CCP", #Cocopah
                                    ifelse(rowid %in% c(50649, 50902), "GM", #Gulf of Mexico
                                           annotation))))

# catch accurate place references for all occurrences of South, Central, and Latin America
## collapse occurrences into one ("Latin America") since historically these have been used interchangeably.
### there is no reference to North America in the corpus
entities <- entities %>% 
  mutate(annotation = ifelse(rowid %in% c(50607, 51786, 120803, 158083, 72509, 177010, 74286, 74120, 165786), "LatAm", #Latin America -- collapsing Latin, Central and South America into one since historically these have been used interchangeably.
                             annotation))

#catch accurate place references for all occurrences of the token "india*"
entities <- entities %>% 
  mutate(annotation = ifelse(rowid %in% c(2464, 2517, 2665, 10491, 13453, 15079, 15093, 26223, 28088, 28141, 28260, 28278, 28290, 31092, 36942, 39772, 50293, 61135, 66134, 69565, 69597, 69862, 111496, 111525, 111534, 111577, 113029, 115146, 123912, 123921, 124011, 124032, 124040, 124071, 124228, 129988, 145797, 145866, 145883,147124, 147224, 147239, 147259, 147331, 147407, 147914, 148024, 148034, 148068, 148170, 148214, 148302, 165032, 165095, 167661, 171555, 171594, 171606, 171622, 171723, 171745, 171893, 172048, 172076, 172103, 174473, 174485, 176746, 176766, 177785, 177808, 177831, 177959, 178119, 178176, 179607, 191035, 191075, 191099, 191102, 191150, 194579, 194621, 203180, 203201, 203311), "NTV", #General meaning of Native
                             ifelse(rowid %in% c(10453, 10476, 10540), "AZ", #Pima, Americopa, and Papago
                                    ifelse(rowid %in% c(2493, 177261, 177284, 177346, 186834), "CCP", #Cocopah
                                           ifelse(rowid %in% c(34598, 169775), "KWK", #Kwakiutl
                                                  ifelse(rowid %in% c(17049, 84646), "MQ", #Moqui
                                                         ifelse(rowid %in% c(2698, 48361), "SX",#Sioux
                                                               ifelse(rowid %in% c(18054, 51474, 71576, 104020, 130205, 130705, 131668, 166739, 166853, 178567, 179626, 180167, 180179, 186504, 186535, 197732, 197864, 201446), "IN", #Indiana
                                                                      ifelse(rowid %in% c(147312), "PB", #Pueblo
                                                                             ifelse(rowid %in% c(164017), "IMD", #India
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
                                                                         ifelse(annotation == "LatAm", "latin_america",
                                                                         ifelse(annotation == "NTV", "native",
                                                                         ifelse(annotation == "AZ", "pima_americopa_papago",
                                                                                ifelse(annotation == "KWK", "kwakiutl",
                                                                                       ifelse(annotation == "MQ", "moqui",
                                                                                              ifelse(annotation == "IN", "indiana",
                                                                                                     ifelse(annotation == "SX", "sioux",
                                                                                                            ifelse(annotation == "PB", "pueblo",
                                                                                                            ifelse(annotation == "IMD", "india",
                                                                                                                   token)))))))))))))))))) 

#### Data Cleaning ####

#remove rows with token "india*" that should not be counted as entities
entities <- entities %>% 
  filter(!rowid == 55872) # "indian summer"

# second count -- this time, without grouping by entity type
entities.count <- entities %>%
  count(token, sort = TRUE) %>% 
  as.data.frame() #use this df to retrieve a list of terms that will be used in the code below.

## when the list is complete, apply fct_collapse on the entities df to get proper place_names (NOT on the entities.count df)
## must count again after this step is complete

entities <- entities %>% 
  mutate(place_name = fct_collapse(token, "United States" = c("united", "american", "americans", "america", "americas", "ameri", "americanization"),
                                   "Latin America" = "latin_america",
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
                                   "United Kingdom" = c("british", "britain", "england", "kingdom", "britannia", "scottish", "scotland"),
                                   "Ireland" = c("irish", "irishmans"),
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
                                   "Philippines" = c("filipino", "filipinos", "fillpino","philippine","philippines","philip", "philipines","philipipne", "philippiies", "philipping","philippne", "filipines", "filipind", "filipinas", "filipine", "filipiso", "filipiz", "filiplines", "visayan", "visayans", "visaran", "visayas", "visaydn", "manila", "igorrotes", "igorrote", "igorrcte"),
                                   "Oklahoma" = "oklahoma",
                                   "Austria-Hungary" = c("austrohungarian", "austrian", "austria", "hungary"),
                                   "Berlin" = "berlin",
                                   "San Francisco" = c("san", "francisco"),
                                   "New Orleans" = c("orleans", "new_orleans"),
                                   "Belgium" = c("belgium", "belgian"),
                                   "Cuba" = c("cuban", "cuba"),
                                   "Virginia" = "virginia",
                                   "Netherlands" = c("dutch", "netherlands", "holland"),
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
                                   "Spain" = c("spanish", "spain"),
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
                                   "Vienna" = "vienna",
                                   "Alton" = "alton",
                                   "Los Angeles" = "angeles",
                                   "Argentina" = "argentine",
                                   "Asia" = c("asian", "asians"),
                                   "Baltimore" = "baltimore",
                                   "Birmingham" = "birmingham",
                                   "Bridgeton" = "bridgeton",
                                   "Cairo" = "cairo",
                                   "Denmark" = "danish",
                                   "New Hampshire" = c("hampshire","new_hampshire"),
                                   "Idaho" = "idaho",
                                   "New Jersey" = c("jersey", "new_jersey"),
                                   "Muscogee Nation" = "muskogee",
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
                                   "Hopi" = c("mokis", "moki", "moqui"),
                                   "Pima-Maricopa" = "pima_americopa_papago",
                                   "Kwakiutl" = "kwakiutl",
                                   "Richmond" = "richmond",
                                   "Venice" = "venice",
                                   "Vermont" = "vermont",
                                   "Wisconsin" = "wisconsin"
  ))

entities$place_name <- entities$place_name %>% 
  as.character()
# based on the examination of the entities.count df, list tokens that must be removed from the entities df
## if a token has less than 2 counts in the entities.count df, no need to include it in the false.tokens list
### make sure to include "states" to avoid double counting "united states" when summarizing counts of "united" and "states"
#### also remove mentions of "panamerican", spanishamerican", and "irishamerican" as, after close reading, these should not be assigned a place name
false.tokens <- c("st", "louis", "louisiana", "louisi", "louislana", "loulsiana", "the", "city", " ", "  ", "states", "native", "mans", "delmar", "english", "east", "la", "west", "jefferson", "plaza", "republican", "al", "columbian", "dem", "dian", "sa", "smoking", "south", "womans", "vt", "christian", "county", "day", "democrat", "jand", "los", "louls", "loutsiana", "mo", "of", "panamerican", "us", "and", "bt", "coast", "democrats", "fu", "fort", "guards", "gulf_mexico", "kas", "medora", "nese", "north", "officiais", "reardon", "republicans", "sea", "sean", "smith", "sou", "thorpe", "to", "toledo", "willis", "   ", "12", "yt", "spanishamerican", "irishamericans", "panamerican", "hamburgamerican", "angloamerican", "germanamerican") 

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

data(world.cities) #get cities
world.cities <- world.cities %>% 
  mutate(name = ifelse(grepl("Havanna", name), "Havana", name)) #fix typo

world.countries <- map_data("world") #get countries
world.countries <- world.countries %>% 
  mutate(region = ifelse(region == "USA", "United States", region)) #fix US name

cities <- c((world.cities$name), "Alton", "Bridgeton", "Washington DC", "Venice")
states <- state.name
countries <- c(unique(world.countries$region), "United Kingdom", "Austria-Hungary", "Persia") 
continents <- c("Latin America", "Africa", "Asia", "Europe")
native_groups <- c("Kwakiutl", "Sioux", "Hopi", "Iroquois", "Cocopah", "Pima-Maricopa", "Muscogee Nation")
geo_regions <- c("Patagonia", "Inner Mongolia")

#create a new variable in the placenames df that distinguishes tokens by scale 
#(i.e., city, state, country, continent, native_nation)
placenames <- placenames %>% 
  mutate(scale = ifelse(place_name %in% cities, "city",
                        ifelse(place_name %in% states, "state",
                               ifelse(place_name %in% countries, "country",
                                      ifelse(place_name %in% continents, "continent",
                                             ifelse(place_name %in% native_groups, "native_group",
                                                    ifelse(place_name %in% geo_regions, "geo_region",
                                                    "NA")))))))

#go in the placenames data frame and fix eventual errors in the scale column
placenames <- placenames %>% 
  mutate(scale = ifelse(place_name %in% c("China", "Mexico"), "country",
                        ifelse(place_name == "Asia", "continent",
                               ifelse(place_name == "Virginia", "state",
                                      ifelse(place_name == "Montana", "state",
                                             ifelse(place_name == "Arizona", "state",
                                      scale))))))

#### Geocoding ####

#make the data frame wider in order to geocode

placenames <- placenames %>% 
  mutate(rowid = row_number())

wide.placenames <- placenames %>%
  pivot_wider(names_from = scale, values_from = place_name) %>% 
#reorder columns per scale
  select(rowid, count, city, state, country, native_group, continent, geo_region)

wide.placenames <- left_join(wide.placenames, placenames) %>% 
  select(rowid, count, city, state, country, native_group, continent, geo_region, place_name)

#fill city column
wide.placenames <- wide.placenames %>% 
  mutate(city = case_when(
    place_name == "United States" ~ "Washington DC",
    place_name == "Philippines" ~ "Metro Manila",
    place_name == "Missouri"~ "Jefferson City",
    place_name == "Germany" ~ "Berlin",
    place_name == "Japan" ~ "Tokio",
    place_name == "France" ~ "Paris",
    place_name == "China" ~ "Beijing",
    place_name == "Mexico" ~ "Mexico City",
    place_name == "United Kingdom" ~ "London",
    place_name == "Kansas" ~ "Topeka",
    place_name == "Texas" ~ "Austin",
    place_name == "California" ~ "Sacramento",
    place_name == "Ireland" ~ "Dublin",
    place_name == "Russia" ~ "Moscow",
    place_name == "Brazil" ~ "Brasilia",
    place_name == "Italy" ~ "Rome",
    place_name == "Ohio" ~ "Columbus",
    place_name == "Canada" ~ "Ottawa",
    place_name == "Indiana" ~ "Indianapolis",
    place_name == "Austria-Hungary" ~ "Vienna",
    place_name == "Netherlands" ~ "The Hague",
    place_name == "Alaska" ~ "Juneau",
    place_name == "Belgium" ~ "Brussels", 
    place_name == "Illinois" ~ "Springfield",
    place_name == "Kentucky" ~ "Frankfort",
    place_name == "Colorado" ~ "Denver",
    place_name == "Cuba" ~ "Havana",
    place_name == "Oklahoma" ~ "Oklahoma City",
    place_name == "Virginia" ~ "Richmond",
    place_name == "New Jersey" ~ "Trenton",
    place_name == "Sweden" ~ "Stockholm",
    place_name == "Arkansas" ~ "Little Rock",
    place_name == "Utah" ~ "Salt Lake City",
    place_name == "Montana" ~ "Helena",
    place_name == "Pennsylvania" ~ "Philadelphia",
    place_name == "Iowa" ~ "Des Moine",
    place_name == "Spain" ~ "Madrid",
    place_name == "Arizona" ~ "Phoenix",
    place_name == "Georgia" ~ "Atlanta",
    place_name == "Maine" ~ "Augusta",
    place_name == "Maryland" ~ "Annapolis",
    place_name == "Nevada" ~ "Carson City",
    place_name == "New Hampshire" ~ "Concord",
    place_name == "Persia" ~ "Tehran",
    place_name == "Syria" ~ "Damascus",
    place_name == "Connecticut" ~ "Hartford",
    place_name == "Minnesota" ~ "Saint Paul",
    place_name == "Mississippi" ~ "Jackson",
    place_name == "Norway" ~ "Oslo",
    place_name == "Oregon" ~ "Salem",
    place_name == "Argentina" ~ "Buenos Aires",
    place_name == "Denmark" ~ "Copenhagen",
    place_name == "Idaho" ~ "Boise",
    place_name == "Nicaragua" ~ "Managua",
    place_name == "Patagonia" ~ "Santa Cruz",
    place_name == "Portugal" ~ "Lisbon",
    place_name == "Switzerland" ~ "Bern",
    place_name == "Vermont" ~ "Montpelier",
    place_name == "Wisconsin" ~ "Madison",
    place_name == "Zimbabwe" ~ "Harare",
    place_name == "Muscogee Nation" ~ "Muscogee (Creek) Nation",
    .default = city))
#fill state column
wide.placenames <- wide.placenames %>% 
  mutate(state = case_when(
    place_name %in% c("Alton", "Cairo", "Springfield", "Chicago") ~ "Illinois",
    place_name == "Atlanta" ~ "Georgia",
    place_name == "Baltimore" ~ "Maryland",
    place_name == "Boston" ~ "Massachusetts",
    place_name == "Bridgeton" ~ "New Jersey",
    place_name == "Cincinnati" ~ "Ohio",
    place_name %in% c("Dallas", "Houston") ~ "Texas",
    place_name == "Denver" ~ "Colorado",
    place_name %in% c("San Francisco", "Sacramento", "Los Angeles") ~ "California",
    place_name %in% c("Louisville", "Richmond") ~ "Kentucky",
    place_name == "Milwaukee" ~ "Wisconsin",
    place_name == "Minneapolis" ~ "Minnesota",
    place_name == "Nashville" ~ "Tennessee",
    place_name == "New Orleans" ~ "Louisiana",
    place_name == "New York" ~ "New York",
    place_name %in% c("Philadelphia", "Pittsburg") ~ "Pennsylvania",
    place_name == "Birmingham" ~ "Alabama",
    place_name == "Kwakiutl" ~ "British Columbia",
    place_name == "Muscogee Nation" ~ "Oklahoma",
    .default = state
  ))
#fill country column 
wide.placenames <- wide.placenames %>% 
  mutate(country = case_when(
    state %in% states | place_name == c("United States", "Washington DC") ~ "USA",
    place_name == "Paris" ~ "France",
    place_name %in% c("London", "United Kingdom") ~ "UK",
    place_name == c("Toronto", "Kwakiutl") ~ "Canada",
    place_name == "Berlin" ~ "Germany",
    place_name == "Jerusalem" ~ "Israel",
    place_name %in% c("Rome", "Venice") ~ "Italy",
    place_name == "Havana" ~ "Cuba",
    place_name %in% c("Vienna", "Austria-Hungary") ~ "Austria",
    place_name == "Patagonia" ~ "Argentina",
    .default = country
  ))
#fix native_group column
wide.placenames <- wide.placenames %>% 
  mutate(native_group = case_when(
    place_name == "Cocopah" ~ "Cocopah Reservation",
    place_name == "Hopi" ~ "Hopi Reservation",
    place_name == "Sioux" ~ "Cheyenne River Reservation",
    place_name == "Pima-Maricopa" ~ "Salt River Pima-Maricopa Indian Community",
    .default = NULL
  ))
#get address column for geocoding
wide.placenames <- wide.placenames %>% 
  unite("geo_address", city:geo_region, sep = ", ", na.rm = TRUE, remove = FALSE) %>% 
  select(place_name, count, city, state, country, native_group, continent, geo_region, geo_address)
#store wide.placenames as dataforgeocoding.csv
write.csv(wide.placenames, "data/dataforgeocoding.csv")

#geocode place names with Google

register_google(key = Sys.getenv("GOOGLEGEOCODE_API_KEY"))

geocoded.data <- geocode(wide.placenames, address = geo_address, method='google', lat = latitude, long = longitude)
#store geocoded data in a csv file
write.csv(geocoded.data, "data/google_geodata.csv")
rm(wide.placenames)

#final cleaning up

geocoded.data <- geocoded.data %>% 
  select(place_name, count, geo_address, latitude, longitude)
#add scale column
geocoded.data <- left_join(geocoded.data, placenames) %>% 
  select(rowid, place_name, count, geo_address, scale, latitude, longitude)
#store geocoded_placenames.csv
write.csv(geocoded.data, "data/geocoded_placenames.csv")
