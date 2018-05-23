rm(list = ls())
source("Functions2.R")

AllDrugsDescriptions<-AllDrugsDescriptions$Item_Description_sep

AllDrugsDescriptions<-AllDrugsDescriptions[1:109690]
AllDrugsCategory<-AllDrugsCategory$Category

AllDrugs<-data.frame(AllDrugsDescriptions, AllDrugsCategory)
names(AllDrugs)=c("Description", "Category")
AllDrugs$Description=as.character(AllDrugs$Description)
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Cannabis/W"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Dissociatives/PCP"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Barbiturates"),]


AllDrugs=AllDrugs[which(substr(AllDrugs$Category,1,6)=="Drugs/"),]


AllDrugs=AllDrugs[which(nchar (AllDrugs$Description)>202),]


Drugs=AllDrugs$Description
Drugs=as.character(Drugs)



names(NonDrugs)=c("Description")
NonDrugs$Description=as.character(NonDrugs$Description)
NonDrugs=NonDrugs[which(nchar (NonDrugs$Description)>180),]
NonDrugs=as.character(NonDrugs)
LegalVector<- vector(mode="numeric", length=length(NonDrugs))


IllegalVector<- vector(mode="numeric", length=length(Drugs))
IllegalVector=rep(0,19579)
LegalVector=rep(1,19962)

Everything=c(Drugs,NonDrugs)
LegalIllegal=c(IllegalVector,LegalVector)
MyData=data.frame(Everything,LegalIllegal)
names(MyData)=c("Description", "Legality")



DirtyDescriptions=MyData$Description
#I need a corpus
DirtyDescriptions_source <- VectorSource(DirtyDescriptions)
DirtyDescriptions_corpus <- VCorpus(DirtyDescriptions_source)
#A clean corpus
cleandrugs_corp <- clean_corpus(DirtyDescriptions_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrug_dtm <- DocumentTermMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf))
#I hate useless words
cleandrug_dtm <- removeSparseTerms(cleandrug_dtm, 0.995)

cleandrug_tdm <- TermDocumentMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf))

