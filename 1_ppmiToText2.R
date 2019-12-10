
require(tm)      || stop("tm support is absent")
require(lsa)     || stop("lsa support is absent")
require(ggplot2) || stop("ggplot2 support is absent")
require(factoextra) || stop("factoextra support is absent")

rm(list=ls())
setwd("C:/Users/vince/Desktop/Tesi/")

bcreatefiles <<- TRUE
bpreprocessing <<- TRUE
bremoveemptyfile <<- TRUE
blsa <<- FALSE
bcluster <<- FALSE
bplot <<- FALSE

run <- function()
{
  cat("Start\n") 
  graphics.off()
  oldw <- getOption("warn")
  options(warn = -1)
  
  tables <- "Tables";
  patients <- "Patient_Status2.csv"
  res <- try(Process(getwd(), tables, patients))
  closeAllConnections()
  if(inherits(res,"try-error"))
    return(as.character(res))
  
  options(warn = oldw)
  cat("Stop\n") 
  return("true")
}

Process <- function(currentdir, tables, patients) {  
  pathLog <- file.path(currentdir,"log.txt");
  unlink(pathLog)
  logfile <- file(pathLog, "w")
  
  cat("Start:",format(Sys.time(), "%d %b %Y %X "),"\n", sep="", file = logfile)  
  
  corpusName <- "Corpus"
  corpusDir <- file.path(currentdir,corpusName)
  
  if(bcreatefiles) {
    #create corpus directory
    cat("Create corpus directory: '",corpusName,"'\n", sep="", file = logfile)
    unlink(corpusDir, recursive=TRUE)
    dir.create(corpusDir, showWarnings = FALSE)
  }
  #load data
  patientsFile <- file.path(currentdir,patients)
  cat("Load data about patients in: '",patientsFile,"'\n", sep="", file = logfile)
  patnos <- processPatients(patientsFile, logfile)

  #patnos <- c("3000","3001", "3002") #remove
  
  tablesFile <- file.path(currentdir,tables)    
  cat("Load data about tables in: '",tablesFile,"'\n", sep="", file = logfile)
  tables <- processTables(tablesFile, logfile)
  
  processPatientsTables(currentdir, corpusDir, patnos, tables, logfile)
}

processPatients <- function(patients, logfile) {
  patno <- read.table(patients,
                               header = TRUE, 
                               sep = ";", 
                               stringsAsFactors = FALSE,
                               fileEncoding = "UTF-8-BOM" ) 

  sortpatno <- sort(unique(patno$PATNO))
  cat("Unique patient numbers: ",length(sortpatno),"\n", sep="", file = logfile)
  return(sortpatno)
}

processTables <- function(dataset, logfile) {
  csvFiles <- list.files(dataset, pattern = ".csv$", full.names = TRUE)
  cat("Data tables: ",length(csvFiles),"\n", sep="", file = logfile)
  return(csvFiles)
}

processPatientsTables <- function(currentdir, corpusdir, patno, tables, logfile) {
  cat("Process patients/tables in: '",corpusdir,"'\n", sep="", file = logfile)
  len <- length(patno)
  
  if(bcreatefiles) {
    cat("Create ",len," output text files in: '",corpusdir,"'\n", sep="", file = logfile)
    for(i_p in patno) {
      patnoFile <- file(file.path(corpusdir, paste(i_p,".txt",sep="")), "w")
      close(patnoFile)
    }
    
    count <- 0
    for(i_p in patno) {
      count <- count + 1
      cat(count,"/",len,"\n",sep="")
      for(j_t in tables) {
        #cat("Analyze patient ",i_p," in file '",j_t,"'\n", sep="", file = logfile)
        datas <- read.csv(j_t, sep = ";",
                          header = TRUE, 
                          stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8-BOM" ) 
        idatas <- which(datas$PATNO == i_p) ## cambiare le visite qui, c("SC","BL")
       # idatas<-which(datas$EVENT)
        subidatas <- datas[idatas,]
        patnoFile <- file(file.path(corpusdir, paste(i_p,".txt",sep="")), "a")
        cat(as.matrix(subidatas)," ",sep=" ", file = patnoFile)
        close(patnoFile)
      }
    }
  }
  
  corpusdirST <- file.path(currentdir,"CorpusST")
  
  if(bpreprocessing) {
    unlink(corpusdirST, recursive = TRUE)
    dir.create(corpusdirST, showWarnings = FALSE)
    
    cat("Preprocessing ",len," output text files in: '",corpusdir,"'\n", sep="", file = logfile)  
    docs_corpus <- VCorpus(DirSource(corpusdir,encoding = "UTF-8"))
    
    docs_corpus <- tm_map(docs_corpus, PlainTextDocument)
    
    tokenize <- function(x) gsub("([_-])", " ", x)
    docs_corpus <- tm_map(docs_corpus, tokenize)        
    
    docs_corpus <- tm_map(docs_corpus, tolower)  #aggiungere i termini che ho trovato
    docs_corpus <- tm_map(docs_corpus, removeWords, c("parkinsonian","parkinsonism","parkinson","pd","parkinsan","parkingsons","parkisons"
                                                      ,"parkinons","parkinsonã½","gparkinson","parkisonism","parkinsinism","parkisnons","parksinsons",
                                                      "parkinsins","parksinons","carbidopalevoparkinsona","parkinosns","parkinsns","parkinspons",
                                                      "parkinssons","parkisnonism","parkisonsons","parknsons","steadyparkinsonirsadapine","parkinsonã½",
                                                      "gparkinson","carbidopalevoparkinsona","steadyparkinsonirsadapine","prkinson"))
                          
    #carbidopa e levdopa sono importate
    docs_corpus <- tm_map(docs_corpus, removePunctuation)   
    docs_corpus <- tm_map(docs_corpus, removeNumbers)
    docs_corpus <- tm_map(docs_corpus, stripWhitespace)
    docs_corpus <- tm_map(docs_corpus, PlainTextDocument)
    
    removeMinWordLength <- function(x) gsub("\\b[[:alpha:]]{1,3}\\b", "", x, perl=T)
    docs_corpus <- tm_map(docs_corpus, removeMinWordLength)    
    docs_corpus <- tm_map(docs_corpus, removeWords, stopwords("english"))   
    docs_corpus <- tm_map(docs_corpus, removeWords, c("log")) 
    docs_corpus <- tm_map(docs_corpus, stripWhitespace)
    
    docs2<-docs_corpus
    docs2<- tm_map(docs2, PlainTextDocument)
    
    #frequenze
    dtmFreq <- TermDocumentMatrix(docs2) 
    m <- as.matrix(dtmFreq)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    write.table(d,file = "frequenzeParole15.11.19.csv",row.names = T,col.names = T)
    
    
    #stemming
    docs_corpus <- tm_map(docs_corpus, stemDocument)   
    docs_corpus <- tm_map(docs_corpus, PlainTextDocument)
    
    fl <- basename(DirSource(corpusdir)$filelist) 
    writeCorpus(docs_corpus, path = corpusdirST, filenames = fl)
  }
  
  if(bremoveemptyfile) {
    cat("Remove empty files from corpus\n",sep="", file = logfile)  
    efiles <- list.files(corpusdirST, pattern = ".txt$", full.names = TRUE)
    for(e_f in efiles) {
      if(file.info(e_f)$size <= 1) {
        cat("Remove file: '",e_f,"'\n",sep="", file = logfile)  
        unlink(e_f)
      }
    }
  }
  
  if(blsa) {
    docs_corpus <- VCorpus(DirSource(corpusdirST))
    cat("Compute Term-Document matrix\n",sep="", file = logfile)  
    dtm <- TermDocumentMatrix(docs_corpus) 
    cat("Compute Tf-Idf\n",sep="", file = logfile) 
    wdtm <- weightTfIdf(dtm, normalize = TRUE)
  
    wtdm.matrix <- as.matrix(wdtm)
    
    
   
    
    cat("Compute LSA\n",sep="", file = logfile) 
    lsaSpace <- lsa(wtdm.matrix, dims=dimcalc_share())
  
    cat("Compute cosine\n",sep="", file = logfile) 
    dLSA <- 1 - cosine(as.textmatrix(lsaSpace))
    
    cat("Normalize distance space\n",sep="", file = logfile) 
    dLSA[which(dLSA < 0)] <- 0
    #dLSA <- dLSA/max(dLSA)
    
    saveRDS(dLSA, file.path(currentdir,"dLSA.rds"))
  }
  
  if(bcluster) {
    cat("Perform k-means clustering\n",sep="", file = logfile) 
    
    dLSAr <- readRDS(file.path(currentdir,"dLSA.rds"))
    
    rows <- nrow(dLSAr)
    k <- min(round(rows/2), 7) #7 is the number of patient classes
    
    kmeans_res <- kmeans(as.matrix(dLSAr), k)
    
    while(length(which(kmeans_res$size <=1)) > 0) {
      toremove <- length(which(kmeans_res$size <=1))
      cat("\nk-means K:",k," (clusters with unique element:",toremove ,")\n")
      print(kmeans_res$size)
      
      k <- k - max(round(toremove/2), 1)
      kmeans_res <- kmeans(as.matrix(dLSAr), k)      
    }
    
    saveRDS(kmeans_res, file.path(currentdir,"kmeans.rds"))
  }

  if(bplot) {  
    clusters <- readRDS(file.path(currentdir,"kmeans.rds"))
    datas <- as.matrix(readRDS(file.path(currentdir,"dLSA.rds")))
    
    fviz_cluster(clusters, data = datas, geom= c("point"))

  }
}

###############
run()
