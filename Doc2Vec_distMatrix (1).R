
require(data.table)
require(dplyr)
library(readr)
library(text2vec)
#da rivedere
rm(list=ls())

f3 <- function(n,list){
  df <- data.frame(id= character(n),sentiment= numeric(n),review=character(n),stringsAsFactors = FALSE)


  for(i in 1:n){
    fileName <- list[i]
    temp_data <- readChar(fileName, file.info(fileName)$size)
    
    df$id[i] <- toString(list[i])
    df$sentiment[i] <- 1
    df$review[i] <- temp_data
  }
  df
}


setwd("C:/Users/vince/Desktop/Tesi/Corpus")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/vince/Desktop/Tesi/Corpus")#devono essere uguali i path

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable

#had to specify columns to get rid of the total column
pazienti<-f3(length(file_list),file_list)

  

doc_set_1 = pazienti
it1 = itoken(doc_set_1$review, progressbar = FALSE)

  
it = itoken(movie_review$review, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)

dtm1 = create_dtm(it1, vectorizer)
dim(dtm1)
#calcolo dissimilarità
d1_d2_tfidf_cos_sim = 1-sim2(x = dtm1, method = "cosine", norm = "l2")
rownames(d1_d2_tfidf_cos_sim)<-file_list
colnames(d1_d2_tfidf_cos_sim)<-file_list

#normalizzo
distMatrix<- as.data.frame(apply(d1_d2_tfidf_cos_sim, 2, function(x) (x - min(x))/(max(x)-min(x))))
save(distMatrix,file = "../distMatrixDoc2Vec.RData")
write.table(distMatrix,file ="distMatrixDoc2Vec.csv",col.names = T,row.names = T)
