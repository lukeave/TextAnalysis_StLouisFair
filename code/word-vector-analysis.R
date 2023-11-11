#install package
if (!require(wordVectors)) {
  if (!(require(devtools))) {
    install.packages("devtools")
  }
  devtools::install_github("bmschmidt/wordVectors")
}
#load libraries
library(wordVectors)
library(magrittr)

#build up / train the model

if (!file.exists("word_embbedings/fullcorpus_bngrams.txt")) prep_word2vec(origin="txt_files/",destination="word_embbedings/fullcorpus_bngrams.txt",lowercase=T,bundle_ngrams=2)

if (!file.exists("word_embbedings/fullcorpus_bngrams.bin")) {model = train_word2vec("word_embbedings/fullcorpus_bngrams.txt","word_embbedings/fullcorpus_bngrams.bin",vectors=200,threads=4,window=30,iter=30,negative_samples=0)} else model = read.vectors("word_embbedings/fullcorpus_bngrams.bin")

model %>% closest_to("indian")

model %>% 
  closest_to(model[[c("philippine","philippines","filipino","igorrote","manila","igorrot","filipinos")]],50)

model %>% 
  closest_to(model[[c("indian","native","tribe","muskogee","sioux","cocopah","indians")]],50)


#visualize indian model
indian = closest_to(model,model[[c("indian","native","tribe","muskogee","sioux","cocopah","indians")]],150)
average_indian = model[[indian$word,average=F]]
plot(average_indian,method="pca")

#visualize philippine model
philippines = closest_to(model,model[[c("philippine","philippines","filipino","igorrote","manila","igorrot","filipinos")]],150)
average_philippines = model[[philippines$word,average=F]]
plot(average_philippines,method="pca")


## trying again without bundling ngrams

#build up / train the model
if (!file.exists("word_embbedings/fullcorpus.txt")) prep_word2vec(origin="txt_files/",destination="word_embbedings/fullcorpus.txt",lowercase=T)

if (!file.exists("word_embbedings/fullcorpus.bin")) {model = train_word2vec("word_embbedings/fullcorpus.txt","word_embbedings/fullcorpus.bin",vectors=200,threads=4,window=30,iter=30,negative_samples=0)} else model = read.vectors("word_embbedings/fullcorpus.bin")

model %>% closest_to("indian")

model %>% 
  closest_to(model[[c("philippine","philippines","filipino","igorrote","manila","igorrot","filipinos")]],50)

model %>% 
  closest_to(model[[c("indian","native","tribe","muskogee","sioux","cocopah","indians")]],50)


#visualize indian model
indian = closest_to(model,model[[c("indian","native","tribe","muskogee","sioux","cocopah","indians")]],150)
average_indian = model[[indian$word,average=F]]
plot(average_indian,method="pca")

#visualize philippine model
philippines = closest_to(model,model[[c("philippine","philippines","filipino","igorrote","manila","igorrot","filipinos")]],150)
average_philippines = model[[philippines$word,average=F]]
plot(average_philippines,method="pca")

#clustering across the full corpus (like topic modeling but less sophisticated)
set.seed(10)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 40)

sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

#clustering based on 5 main topics
topics = c("philippine","indian","sioux","cocopah", "igorrote")
term_set = lapply(topics, 
                  function(topic) {
                    nearest_words = model %>% closest_to(model[[topic]],20)
                    nearest_words$word
                  }) %>% unlist

subset = model[[term_set,average=F]]

subset %>%
  cosineDist(subset) %>% 
  as.dist %>%
  hclust %>%
  plot