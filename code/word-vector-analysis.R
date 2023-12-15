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
library(dplyr)
library(ggplot2)


#### Building the Model ####


if (!file.exists("word_embeddings/fullcorpus_bngrams.txt")) prep_word2vec(origin="txt_files/",destination="word_embeddings/fullcorpus_bngrams.txt",lowercase=T,bundle_ngrams=2)

if (!file.exists("word_embeddings/fullcorpus_bngrams.bin")) {model = train_word2vec("word_embeddings/fullcorpus_bngrams.txt","word_embeddings/fullcorpus_bngrams.bin",vectors=50,threads=4,window=30,iter=30,negative_samples=0)} else model = read.vectors("word_embeddings/fullcorpus_bngrams.bin")

model <- model[-1,] #remove "</s>" (first row) from the model as it seems to be an OCR error


#### Semantic Similarity ####


model %>% closest_to("savage", 20)

model %>% closest_to("progress", 20)

#visualize semantic similarity of "savage" and "progress" based on the preliminary lists above
modernity = closest_to(model,model[[c("progress", "civilization", "science")]], 60)
vector_modernity = model[[modernity$word,average=F]]
plot(vector_modernity,method="pca")

otherness = closest_to(model,model[[c("savage", "primitive", "barbaric")]], 60)
vector_otherness = model[[otherness$word,average=F]]
plot(vector_otherness,method="pca")

#plot semantic similarity of "savage" and "progress" terms together
vector_otherness_modernity = model[[c(modernity$word, otherness$word),average=F]]
progress_score = vector_otherness_modernity %>% cosineSimilarity(model[[c("progress", "civilization", "science")]])
savagery_score = vector_otherness_modernity %>%  cosineSimilarity(model[[c("savage", "primitive", "barbaric")]])

plot(progress_score, savagery_score, type='n', xlab="Similarity to 'progress' terminology", ylab="Similarity to 'savagery' terminology")
abline(a=0,b=1)
text(progress_score,savagery_score,labels=rownames(vector_otherness_modernity), cex=.7)


####  Clustering topics ####


#clustering across the full corpus (like topic modeling but less sophisticated)
set.seed(15)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 200)

sapply(sample(1:centers,4),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

#following the hint -- clustering based on 2 main topics
topics = c("primitive", "civilization")
term_set = lapply(topics, 
                  function(topic) {
                    nearest_words = model %>% closest_to(model[[topic]], 15)
                    nearest_words$word
                  }) %>% unlist

subset = model[[term_set,average=F]]

subset %>%
  cosineDist(subset) %>% 
  as.dist %>%
  hclust %>%
  plot

#dendrogram - modernity
modernity_distances = cosineDist(vector_modernity,vector_modernity) %>% 
  as.dist %>% 
  hclust %>% 
  plot

#dendrogram - otherness
otherness_distances = cosineDist(vector_otherness, vector_otherness) %>% 
  as.dist %>% 
  hclust %>% 
  plot



# Plotting representations of the American and Filipino identities across the "civilization" spectrum

american_filipino_relations = data.frame(word = rownames(vector_otherness_modernity)) 

american_filipino_vector = model[[c("american", "americans", "america", "united_states")]] - model[[c("philippines", "philippine", "filipino", "moros", "igorrotes", "visayans")]]

progress_savagery_vector = model[[c("progress", "civilization", "science")]] - model [[c("savage", "primitive", "barbaric")]]

american_filipino_relations$american_vs_filipino = cosineSimilarity(vector_otherness_modernity, american_filipino_vector)

american_filipino_relations$progress_vs_savagery = cosineSimilarity(vector_otherness_modernity, progress_savagery_vector)

ggplot(american_filipino_relations, aes(x=american_vs_filipino, y=progress_vs_savagery, label=word)) + geom_text(size=2.5) +
  scale_y_continuous("<------ less civilized ....... more civilized ------->", limits=c(-.45,.25)) +
  scale_x_continuous("<------ Filipino representation ....... American representation ------>", limits=c(-.25,.33))
