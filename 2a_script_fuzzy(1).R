
library(cluster)
library(factoextra)


rm(list=ls())
setwd("C:/Users/vince/Desktop/Tesi/SC-V15") #modifica
load("distMatrixDoc2Vec.RData")



K <- 2
rnames <- row.names(distMatrix)
  
fcluster <- fanny(distMatrix, k = K , maxit = 1000,  diss = TRUE, memb.exp = 1.01, keep.diss = TRUE, 
                   tol = 1e-20) 

save(fcluster, file="fuzzy.RData")

# plot 
#clusplot(distMatrix, fcluster$clustering, color=T, shade=T, labels=4, lines=0)  



Patient_Status<-read.table("Patient_Status.csv",
                     header = TRUE, 
                     sep = ";", 
                     stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8-BOM")  

patstatus <- Patient_Status$RECRUITMENT_CAT 

patstatus[which(patstatus == "PD")] <- "PD"
patstatus[which(patstatus == "PRODROMA")] <- "PD"
patstatus[which(patstatus == "GENPD")] <- "PD"
patstatus[which(patstatus == "REGPD")] <- "PD"

patstatus[which(patstatus == "GENUN")] <- "GENUN"
patstatus[which(patstatus == "HC")] <- "GENUN"
patstatus[which(patstatus == "REGUN")] <- "GENUN"

patstatus <- as.data.frame(patstatus)
patno <- Patient_Status$PATNO
spatno <- paste(as.character(patno),".txt",sep="")
row.names(patstatus) <- spatno
spatno <- as.data.frame(spatno)


th <- 1/K; 
cls <- vector("list",K)  


for(i in 1:K)
{
  cat("Cluster all:",i,length(fcluster$membership[,i]),"\n", sep=" ")
  cls[[i]] <-  as.vector(which(fcluster$membership[,i] > th))
}
cat("\n")
 
varcls <- vector("list",K) 

for(i in 1:K)
{
  cat("Cluster:",i,"number of elements",length(cls[[i]]),"\n", sep=" ")
  #print(rnames[cls[[i]]])
  varcls[[i]] <- patstatus[which(row.names(patstatus) %in% rnames[cls[[i]]]),1]
  print(table(varcls[[i]]))
  cat("\n")
}

# 
# cluster <- fcluster$membership
# cluster <- as.data.frame(cluster)
# save(cluster, file="cluster_fuzzy.RData")
# cluster1 <- rnames[cls[[1]]]
# save(cluster1, file="cluster1_fuzzy.RData")
# 
# cluster2 <- rnames[cls[[2]]]
# save(cluster2, file="cluster2_fuzzy.RData")
# 

#fviz_silhouette(fcluster, palette = "rainbowcols", ggtheme = theme_minimal())


