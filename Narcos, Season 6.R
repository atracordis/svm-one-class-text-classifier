source("Functions, Season 2.R")
#EVALUATION

vec=colnames(X)
SampleDrugs <- read.table("SampleData.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
names(SampleDrugs)=c("Description")
SampleDrugs$Description=as.character(SampleDrugs$Description)
SampleDrugs$Description=stemmer(SampleDrugs$Description)


SampleDrugs$Description=lapply(SampleDrugs$Description, function(x) {
  x <- unlist(strsplit(x, " "))
  x <- x[tolower(x) %in% vec]
  x<-paste(x,collapse=" ")
})
SampleDrugs=SampleDrugs$Description

AllDrugsCategory <- read.table("Category_New.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
AllDrugsDescriptions <- read.table("Description_New.csv",stringsAsFactors = FALSE, header=T,fill=T, encoding="utf-8", sep="²", dec=".",quote = "", na.string="" )
AllDrugsDescriptions<-AllDrugsDescriptions[1:109690,]
AllDrugsDescriptions<-AllDrugsDescriptions$Item.Description.sep
AllDrugsCategory<-AllDrugsCategory$Category
AllDrugs<-data.frame(AllDrugsDescriptions, AllDrugsCategory)
names(AllDrugs)=c("Description", "Category")
AllDrugs$Description=as.character(AllDrugs$Description)
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Cannabis/W"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Dissociatives/PCP"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Barbiturates"),]

AllDrugs=AllDrugs[which(substr(AllDrugs$Category,1,6)=="Drugs/"),]

AllDrugs=AllDrugs[which(nchar (AllDrugs$Description)>202),]


AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Dissociatives/Other"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Dissociatives/GBL"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Psychedelics/Mescaline"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Psychedelics/Salvia"),]
AllDrugs$Category=droplevels(AllDrugs$Category)
new_df <- AllDrugs %>% group_by(Category) %>% sample_n(12)

vec
new_df=new_df$Description
new_df=as.character(new_df)


new_df=stemmer(new_df)

new_df=lapply(new_df, function(x) {
  x <- unlist(strsplit(x, " "))
  x <- x[tolower(x) %in% vec]
  x<-paste(x,collapse=" ")
})

head(new_df)
head(SampleDrugs)
All=c(SampleDrugs, new_df)

IllegalVector<- vector(mode="numeric", length=length(new_df))
IllegalVector=rep(0,length(new_df))

TooLegalVector<- vector(mode="numeric", length=length(SampleDrugs))
TooLegalVector=rep(1,length(SampleDrugs))
Legality=c(TooLegalVector,IllegalVector)


MyDataEval=data.frame(All,Legality)

summary(MyDataEval)
names(MyData)=c("Description", "Legality")


DirtyDescriptions_source <- VectorSource(All)
DirtyDescriptions_corpus <- VCorpus(DirtyDescriptions_source)
cleandrugs_corp <- clean_corpus(DirtyDescriptions_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrug_dtm <- DocumentTermMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf))


target=Legality
y=as.numeric(target)
y=as.factor(y)
X <- as.matrix(cleandrug_dtm)
data <- as.data.frame(cbind(y,X))

mat <- matrix(rep(0,nrow(data)*length(vec)), nrow = nrow(data), ncol =length(vec))

colnames(mat)=vec
mat<-as.data.frame(mat)

columns_to_delete=colnames(as.data.frame(X))

mat<-mat[ , !(names(mat) %in% columns_to_delete)]

data<- as.data.frame(cbind(data,mat))


table(Pred=predict(sv, data[,-1]) , True=data$y)
