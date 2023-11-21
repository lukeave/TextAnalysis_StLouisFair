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

if (!file.exists("word_embeddings/fullcorpus_bngrams.txt")) prep_word2vec(origin="txt_files/",destination="word_embeddings/fullcorpus_bngrams.txt",lowercase=T,bundle_ngrams=2)

if (!file.exists("word_embeddings/fullcorpus_bngrams.bin")) {model = train_word2vec("word_embeddings/fullcorpus_bngrams.txt","word_embeddings/fullcorpus_bngrams.bin",vectors=50,threads=4,window=30,iter=30,negative_samples=0)} else model = read.vectors("word_embeddings/fullcorpus_bngrams.bin")

model <- model[-1,] #remove "</s>" (first row) from the model as it seems to be an OCR error

model %>% closest_to("savage", 20)

model %>% closest_to("progress", 20)

#visualize semantic similarity of "savage" and "progress" based on the preliminary lists above
modernity = closest_to(model,model[[c("progress", "civilization", "science")]], 60)
vectors_modernity = model[[modernity$word,average=F]]
plot(vectors_modernity,method="pca")

otherness = closest_to(model,model[[c("savage", "primitive", "barbaric")]], 60)
vectors_otherness = model[[otherness$word,average=F]]
plot(vectors_otherness,method="pca")

##plot semantic similarity of "savage" and "progress" together
otherness_modernity = closest_to(model, model[[c("savage", "primitive", "barbaric", "progress", "civilization", "science")]],100)
vectors_otherness_modernity = model[[otherness_modernity$word,average=F]]
plot(vectors_otherness_modernity,method="pca")


#clustering across the full corpus (like topic modeling but less sophisticated)
set.seed(15)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 200)

sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

#clustering based on 2 main topics
topics = c("savage", "progress")
term_set = lapply(topics, 
                  function(topic) {
                    nearest_words = model %>% closest_to(model[[topic]],15)
                    nearest_words$word
                  }) %>% unlist

subset = model[[term_set,average=F]]

subset %>%
  cosineDist(subset) %>% 
  as.dist %>%
  hclust %>%
  plot
