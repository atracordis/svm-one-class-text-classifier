library(tm)
library(qdap)
library(e1071)
library(SnowballC)
library(wordcloud)
library(RTextTools)
library(tidytext)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}
library(plyr); library(dplyr)
library(arules)
library(SnowballC)
library(class)
library(wordcloud)
library(igraph)
library(RWeka)
library(slam)
library(NLP)
library(caret)


clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  corpus <- tm_map(corpus, toSpace, "[^a-zA-Z ]")
  corpus <- tm_map(corpus, toSpace, "â")
  corpus <- tm_map(corpus, toSpace, "ª")
  corpus <- tm_map(corpus, toSpace, "œ")
  corpus <- tm_map(corpus, toSpace, "¤")
  
  #weird="âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœª"
  #Remove profane words
  #First get the list of bad words
  #bwdat<-read.table("en_bws.txt", header=FALSE, sep="\n", strip.white=TRUE)
  # names(bwdat)<-"Bad Words"
  #  corpus<- tm_map(corpus, removeWords, bwdat[,1])
  
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("the","day","yes","yet", "like","œ","¤","â","will", "sale","get", "good","also","best","can",  "and", stopwords("english") ))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, removeWords, c("usd","youll","your","collect","huge","flipkartcom","price","order","list","onlin","pleas","ship","buy","com","flipkart","aaa","deliveri","cash","one","india","shop","read","set","print","new","page","come","requir","give","name","know","item","general","red","question","add","final","take","want","see","around","discount","content","two"))
  
  return(corpus)
}



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


AllDrugs=AllDrugs[which(nchar (AllDrugs$Description)>200),]
AllDrugs$Category=droplevels(AllDrugs$Category)



Drugs=AllDrugs$Description
Drugs=as.character(Drugs)

IllegalVector<- vector(mode="logical", length=length(Drugs))
IllegalVector=rep(TRUE,length(Drugs))

MyData=data.frame(Drugs,IllegalVector)
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

target=MyData$Legality
y=as.numeric(target)
y=as.factor(y)
X <- as.matrix(cleandrug_dtm)



data <- as.data.frame(cbind(y,X))
colnames(data[2:length(colnames(data))])
trainPositive<-data[rowSums(data[2:length(colnames(data))]) != 0,]

predictors=setdiff(colnames(trainPositive), c("y"))

inTrain<-createDataPartition(1:nrow(trainPositive),p=0.8,list=FALSE)



SampleDrugs <- read.table("flipkart.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
names(SampleDrugs)=c("Description")
SampleDrugs$Description=as.character(SampleDrugs$Description)
SampleDrugs=SampleDrugs[which(nchar (SampleDrugs$Description)>180),]
SampleDrugs=stemmer(SampleDrugs)

SampleDrugs=lapply(SampleDrugs, function(x) {
  x <- unlist(strsplit(x, " "))
  x <- x[tolower(x) %in% predictors]
  x<-paste(x,collapse=" ")
})


DirtyDescriptions_test_source <- VectorSource(SampleDrugs)
DirtyDescriptions_test_corpus <- VCorpus(DirtyDescriptions_test_source)
cleandrugs_test_corp <- clean_corpus(DirtyDescriptions_test_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrug_test_dtm <- DocumentTermMatrix(cleandrugs_test_corp, control = list(weighting = weightTfIdf))

X_test <- as.matrix(cleandrug_test_dtm)

Legality_Test<- vector(mode="logical", length=length(SampleDrugs))
Legality_Test=rep(FALSE,length(SampleDrugs))

Legality_Test

y_test=as.numeric(Legality_Test)
y_test=as.factor(y_test)
Negative_Test <- as.data.frame(cbind(y_test,X_test))
colnames(Negative_Test)
Negative_Test$y_test<-Negative_Test$y_test-1
Negative_Test$y_test=as.factor(Negative_Test$y_test)

mat <- matrix(rep(0,nrow(Negative_Test)*length(predictors)), nrow = nrow(Negative_Test), ncol =length(predictors))

colnames(mat)=predictors
mat<-as.data.frame(mat)

columns_to_delete=colnames(as.data.frame(Negative_Test))

mat<-mat[ , !(names(mat) %in% columns_to_delete)]

data_test<- as.data.frame(cbind(Negative_Test,mat))

data_test<-data_test[ , (names(data_test) %in% c(predictors,"y_test"))]
dim(data_test)

colnames(data_test)[1] <- "y"

trainpredictors<-trainPositive[inTrain,predictors]
trainLabels<-trainPositive[inTrain,"y"]

testPositive<-trainPositive[-inTrain,]
testnegative<-data_test
dim(testnegative)
dim(testPositive)
testPositive$y

testPosNeg<-rbind(testPositive,testnegative)
testLabels<-testPosNeg[,"y"]
testLabels<-as.numeric(testLabels)

testpredictors<-testPosNeg[,predictors]

svm.model<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.3, scale=TRUE, kernel="radial")
#save(svm.model, file = "svm.model.RData")

#load("svm.model.RData") 


svm.predtrain<-predict(svm.model,trainpredictors)
svm.predtest<-predict(svm.model,testpredictors)

confTrain<-table(Predicted=svm.predtrain,Reference=trainLabels)
confTest<-table(Predicted=svm.predtest,Reference=testLabels)

confusionMatrix(confTest,positive='TRUE')

print(confTrain)
print(confTest)

svm.model.2<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.2, scale=TRUE, kernel="radial")

