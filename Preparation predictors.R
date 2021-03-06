library(readr)
library(tm)
library(qdap)

allDrugSlangs <- read_file("drugs.txt")
drugslangsSource <- VectorSource(allDrugSlangs)
drugSlangsCorpus <- VCorpus(drugslangsSource)
drugSlangs_dtm <- DocumentTermMatrix(drugSlangsCorpus)
drugSlangsList=colnames(as.matrix(drugSlangs_dtm))
refinedDrugSlangsList=tolower(stemmer(drugSlangsList,warn=F))
write.csv(file="Refined_Drug_Slangs.txt",refinedDrugSlangsList)
