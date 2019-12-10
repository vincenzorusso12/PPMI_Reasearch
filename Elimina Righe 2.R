
require(dplyr)

rm(list=ls())


setwd("C:/Users/vince/Desktop/Tesi/SC-LOG")  #modificare ogni volta
visite<-c("SC","BL","V01","V02","V03","V04","V05","V06","V07","V08","V09",
          "V10","V11","V12","V13","V14","V15","ST","LOG") #modificare ogni volta
dir.create("TablesRidotte")
outputPath<-"C:/Users/vince/Desktop/Tesi/SC-LOG/TablesRidotte/" #modificare ogni volta
tablesSource<-"C:/Users/vince/Desktop/Tesi/Tables"





list.files(pattern=".csv$") # use the pattern argument to define a common pattern  for import files with regex. Here: .csv

# create a list from these files
list.filenames<-list.files(tablesSource,pattern=".csv$",full.names = T)
list.titles<-list.files(tablesSource,pattern=".csv$",full.names = F)
list.filenames

# create an empty list that will serve as a container to receive the incoming files
list.data<-list()
list.data2<-list()

# create a loop to read in your data
for (i in 1:length(list.filenames))
{
  list.data[[i]]<-read.csv(list.filenames[i], sep = ";",
                           header = TRUE, 
                           stringsAsFactors = FALSE,
                           fileEncoding = "UTF-8-BOM")
  
  
}

# add the names of your data to the list
names(list.data)<-list.filenames


# carico una singola tabella

# tab<-list.data[[6]]
# tab<-filter(tab,EVENT_ID%in%visite)


#carixco tutte e una a una elimino le righe che non contengono la lista di visite
for (i in 1:length(list.data)) { 
  list.data2[[i]]<-filter(list.data[[i]],EVENT_ID%in%visite)
}
names(list.data2)<-list.filenames

  #processo le tabelle  elimino le righe
                 

for (i in 1:length(list.data)) {
  write.csv2(list.data2[[i]], paste0(outputPath,list.titles[i]),row.names = F) #cancella numero osservazioni

}