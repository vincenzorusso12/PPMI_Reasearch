library(lsa)
library(factoextra)
library(cluster)

rm(list=ls())
setwd("C:/Users/vince/Desktop/Tesi")

#write.table(tab,file="A.csv",quote=F,sep=";",dec=",",na="",row.names=T)
#read.table(...)
#LSA
# myMatrix <- textmatrix("C:/Users/vince/Desktop/Tesi/CorpusST")
# myMatrix <- na.omit(myMatrix)
# save(myMatrix, file="textMatrix.RData")
# 
# myMatrix1 <- lw_bintf(myMatrix) * gw_idf(myMatrix)
# save(myMatrix1, file="textMatrixN.RData")
# 
# myLSAspace <- lsa(myMatrix1, dims=dimcalc_share())
# save(myLSAspace, file="myLSAspace.RData")
# 
# rm(myMatrix1)
# rm(myMatrix)
# 
# distMatrix <- 1 - cosine(as.textmatrix(myLSAspace))
# distMatrix[which(distMatrix < 0)] <- 0
# write.table(distMatrix,file ="distMatrix.csv",col.names = T,row.names = T)
# save(distMatrix, file="distMatrix.RData")
load("distMatrixDoc2Vec.RData")

#CLUSTERING K-MEANS
k<- 2
cluster.kmeans<-kmeans(as.matrix(distMatrix),k)


save(cluster.kmeans, file="kmeans.RData")
#rm(myLSAspace)

# plot
fviz_cluster(cluster.kmeans,data=distMatrix,geom= "point")

# individuazione dei diversi oggetti all'interno del cluster
cluster<- cluster.kmeans$cluster
cluster<- as.data.frame(cluster)
write.csv(cluster,file="clusterkmeans.csv")
save(cluster, file="cluster.RData")
cluster$documenti<-rownames(cluster)
save(cluster,file="cluster.RData")

patients<-read.table("Patient_Status.csv",
                           header = TRUE, 
                           sep = ";", 
                           stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8-BOM")  
patstatus <- patients$RECRUITMENT_CAT 
  
patstatus[which(patstatus == "PD")] <- "PD"
patstatus[which(patstatus == "PRODROMA")] <- "PD"
patstatus[which(patstatus == "GENPD")] <- "PD"
patstatus[which(patstatus == "REGPD")] <- "PD"

patstatus[which(patstatus == "GENUN")] <- "GENUN"
patstatus[which(patstatus == "HC")] <- "GENUN"
patstatus[which(patstatus == "REGUN")] <- "GENUN"

#patstatus<-Patient_Status
patstatus <- as.data.frame(patstatus)
patno <- patients$PATNO
spatno <- paste(as.character(patno),".txt",sep="")
row.names(patstatus) <- spatno

indici<- which(cluster$cluster==1)
indici
cluster1<-cluster$documenti[indici] 
cluster1
cluster1<- as.data.frame(cluster1)
save(cluster1,file="cluster1.RData")
#perche cluster 2 non ha patno.txt
indici<- which(cluster$cluster==2)
indici
cluster2<-cluster$documenti[indici] 
cluster2
cluster2<- as.data.frame(cluster2)
save(cluster2,file="cluster2.RData")
#fin qui gli indici vano bene

th <- 1/k;
for(i in 1:k)
{
  cat("Cluster all:",i,(cluster.kmeans$size[i]),"\n", sep=" ")
  
}
cat("\n")
  
varcls <- vector("list",k) 
cluster<- as.data.frame(cluster.kmeans$cluster)
cluster$documenti <- row.names(cluster)


 cat("Cluster:",1,"number of elements",cluster.kmeans$size[1],"\n", sep=" ")
 
  
  indici<- as.vector(which(cluster$cluster == 1))
  indici
  
  
  cluster1<- cluster[indici,]
  cluster1<- as.data.frame(cluster1)
  documenti <- row.names(cluster1)
  documenti <- as.data.frame(documenti)
  cluster1 <- cluster1[,-2]
  cluster1 <- as.data.frame(cluster1)
  row.names(cluster1) <- documenti$documenti
  cluster1
  
  cat("Cluster:",1,"number of elements",cluster.kmeans$size[1],"\n", sep=" ")
  varcls[[1]] <- patstatus[which(row.names(patstatus) %in% row.names(cluster1)),1]
  
  print(table(varcls[[1]]))
  cat("\n")
  
  
  indici<- as.vector(which(cluster$cluster == 2))
  indici
  
  
  cluster2<- cluster[indici,]
  cluster2<- as.data.frame(cluster2)
  documenti <- row.names(cluster2)
  documenti <- as.data.frame(documenti)
  cluster2 <- cluster2[,-2]
  cluster2 <- as.data.frame(cluster2)
  row.names(cluster2) <- documenti$documenti
  cluster2
  
  cat("Cluster:",2,"number of elements",cluster.kmeans$size[2],"\n", sep=" ")
  varcls[[2]] <- patstatus[which(row.names(patstatus) %in% row.names(cluster2)),1]
  
  print(table(varcls[[2]]))
  cat("\n")
  

 
  library(mclust)
  fit <- Mclust(distMatrix)
  plot(fit) # plot results
  summary(fit) # display the best model
 
  library(fpc)
  plotcluster(distMatrix, cluster.kmeans$cluster)
  library(fpc)
  cluster.stats(d, fit1$cluster, fit2$cluster)
 
  plot(silhouette(cluster.kmeans$cluster, d = distMatrixdim, border=NA))
 
  


