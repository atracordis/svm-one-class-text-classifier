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

column_to_remove=read.csv(file="colnames.csv")
column_to_remove<-column_to_remove$x

clean_normalize <- function(drug_string){
  stemmed_drug_string<-stemmer(drug_string, warn=F)
  stemmed_drug_string<-tolower(stemmed_drug_string) 
  stemmed_drug_string<- stemmed_drug_string[stemmed_drug_string %in% AllPredictors]
  stemmed_drug_string<-paste(stemmed_drug_string,collapse=" ")
  DirtyDescriptions_test_final_source <- VectorSource(stemmed_drug_string)
  DirtyDescriptions_test_final_corpus <- VCorpus(DirtyDescriptions_test_final_source)
  cleandrugs_test_final_corp <- clean_corpus(DirtyDescriptions_test_final_corpus)
  cleandrug_test_final_dtm <- DocumentTermMatrix(DirtyDescriptions_test_final_corpus, 
                                                 control =
                                                   list(removePunctuation=TRUE,
                                                        stopwords=TRUE,
                                                        weighting = weightBin))
  X_test_final <- as.matrix(cleandrug_test_final_dtm)
  normalized_mean<-(sum(X_test_final)-mean(trainRowSums))/sd(trainRowSums)
  normalized<-(sum(X_test_final)-min(trainRowSums))/(max(trainRowSums)-min(trainRowSums))
  return(normalized);
}

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
  corpus <- tm_map(corpus, removeWords, c("dri","line","busi","die","extrem","china","oil","Durban","green","classic","citrus","crush","cheap","old","oral","trust","user","use","mtehod","suppli","lemon","extra","usp","vacuum","trim","straight","satisfact","usd","Dri","Green","Addit","youll","your","collect","huge","flipkartcom","price","order","list","onlin","pleas","ship","buy","com","flipkart","aaa","deliveri","cash","one","india","shop","read","set","print","new","page","come","requir","give","name","know","item","general","red","question","add","final","take","want","see","around","discount","content","two"))
  
  corpus <- tm_map(corpus, removeWords, c("addit", "amnesia", "aroma", "bar", 
                                          "batch", "black", "blister", "blueberri",
    "buzz", "chemic", "chine", "clean", "clear", "cure",
    "dark", "drop", "durban", "dutch", "experi", "experienc", "express", 
    "flavor", "flower", "flush", "fresh", "grow", "grower", "grown", "hard", 
    "hit", "hybrid", "ice", "ingredi","instant", "legendari",
    "light", "limit", "liquid", "local", "love", "magic", "medic", "medicin", "method",
    "natur","nice", "northern", "organ", "ounc", "potenc", "power", "rare", "rate",
    "relax", "safe", "sampl", "sleep", "smell", "soft",
    "strength", "stronger", "stuff", "supplier", "synthet", "thing", "treatment", 
    "wax", "white"))
  corpus <- tm_map(corpus, removeWords, column_to_remove)
  
  
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


AllDrugs=AllDrugs[which(substr(AllDrugs$Category,1,6)=="Drugs/"),]


AllDrugs=AllDrugs[which(nchar (AllDrugs$Description)>200),]




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
X <- as.matrix(cleandrug_dtm)
X<-as.data.frame(X)

y<-target





#X<-X[, !(colnames(X) %in% column_to_remove$x)]
#write.csv(file="colnames.csv",colnames(X ))

data <- as.data.frame(cbind(y,X))
colnames(data[2:length(colnames(data))])


trainPositive<-data[rowSums(data[2:length(colnames(data))]) != 0,]
NaturalPredictors=setdiff(colnames(trainPositive), c("y"))

inTrain<-createDataPartition(1:nrow(trainPositive),p=0.8,list=FALSE)



trainpredictors<-trainPositive[inTrain,NaturalPredictors]
trainLabels<-trainPositive[inTrain,"y"]


table(trainLabels)

trainRowSums=rowSums(trainpredictors[rowSums(trainpredictors) > quantile(rowSums(trainpredictors), 0.1),])
trainTarget=trainLabels[rowSums(trainpredictors) > quantile(rowSums(trainpredictors), 0.1)]



#HERE IT STARTS
SampleDrugs <- read.table("flipkart.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
names(SampleDrugs)=c("Description")
SampleDrugs$Description=as.character(SampleDrugs$Description)
SampleDrugs=SampleDrugs[which(nchar (SampleDrugs$Description)>180),]
#SampleDrugs=stemmer(SampleDrugs)
SampleDrugs=lapply(SampleDrugs, FUN=stemmer,warn=F)

DEAPredictors=read.csv(file="Refined_Drug_Slangs.txt")

AllPredictors=c(as.character(DEAPredictors$x), NaturalPredictors)

SampleDrugs=lapply(SampleDrugs, function(x) {
  x <- unlist(strsplit(x, " "))
  x <- x[tolower(x) %in% AllPredictors]
  x<-paste(x,collapse=" ")
})


DirtyDescriptions_test_source <- VectorSource(SampleDrugs)
DirtyDescriptions_test_corpus <- VCorpus(DirtyDescriptions_test_source)
cleandrugs_test_corp <- clean_corpus(DirtyDescriptions_test_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrug_test_dtm <- DocumentTermMatrix(cleandrugs_test_corp, control = list(weighting = weightTfIdf))

X_test <- as.matrix(cleandrug_test_dtm)

#HERE IT STOPS




colnames(X_test)

summary(X_test)

Legality_Test<- vector(mode="logical", length=length(SampleDrugs))
Legality_Test=rep(FALSE,length(SampleDrugs))


y_test<-Legality_Test

Negative_Test <- as.data.frame(cbind(y_test,X_test))
Negative_Test$y_test<-as.logical(Negative_Test$y_test)
mat <- matrix(rep(0,nrow(Negative_Test)*length(NaturalPredictors)), nrow = nrow(Negative_Test), ncol =length(NaturalPredictors))
colnames(mat)=NaturalPredictors
mat<-as.data.frame(mat)

columns_to_delete=colnames(as.data.frame(Negative_Test))
colnames(Negative_Test)

mat<-mat[ , !(names(mat) %in% columns_to_delete)]
colnames(mat)
data_test<- as.data.frame(cbind(Negative_Test,mat))
data_test<-data_test[ , (names(data_test) %in% c(NaturalPredictors,"y_test"))]
dim(data_test)

colnames(data_test)[1] <- "y"

testPositive<-trainPositive[-inTrain,]
testnegative<-data_test
dim(testnegative)
dim(testPositive)

testPosNeg<-rbind(testPositive,testnegative)
testLabels<-testPosNeg[,"y"]
table(testLabels)

testpredictors<-testPosNeg[,NaturalPredictors]

svm.model<-svm(scale(trainRowSums),y=NULL, type='one-classification', nu=0.001,gamma=0.01, scale=TRUE, kernel="radial")
#save(svm.model, file = "svm.model.RData")

tuned <- tune.svm(x=trainRowSums, y=trainTarget, 
                  nu =  0.001:0.5,
                  gamma = 10^(-2:0),
                  type='one-classification'                 
);
tuned
#load("svm.model.RData") 


svm.predtrain<-predict(svm.model,scale(trainRowSums))
svm.predtest<-predict(svm.model,(rowSums(testpredictors)-mean(trainRowSums))/sd(trainRowSums))



confTrain<-table(Predicted=svm.predtrain,Reference=trainTarget)
confTest<-table(Predicted=svm.predtest,Reference=testLabels)

print(confTrain)
print(confTest)

accuracy=sum(diag(confTest))/sum(confTest)
precision=confTest[4]/(confTest[4]+confTest[2])
recall=confTest[4]/(confTest[4]+confTest[3])
F1_Score=2*(precision*recall)/(recall+precision)

library(ROCR)  

m3_1_pred <- prediction( as.numeric(svm.predtest) , as.numeric(testLabels))
m3_1_perf <- performance(m3_1_pred, "tpr", "fpr")


plot(m3_1_perf,colorize=TRUE, lwd=2, main = "m3_1 ROC: SVM ONE CLASS", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)


#library(devtools)
#install_github('benmack/oneClass')

drug_string="I'm selling some pretty sweet zacateca 
if you're interested call me it's gonna be xtreme, got some weird seed, oxycontin
potent and pure heroin, good kush coming straight from holland man"
drug_string="zacateca"

stemmed_drug_string<-stemmer(drug_string, warn=F)

stemmed_drug_string<-tolower(stemmed_drug_string) 
stemmed_drug_string<- stemmed_drug_string[stemmed_drug_string %in% AllPredictors]
stemmed_drug_string<-paste(stemmed_drug_string,collapse=" ")

DirtyDescriptions_test_final_source <- VectorSource(stemmed_drug_string)
DirtyDescriptions_test_final_corpus <- VCorpus(DirtyDescriptions_test_final_source)
cleandrugs_test_final_corp <- clean_corpus(DirtyDescriptions_test_final_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrug_test_final_dtm <- DocumentTermMatrix(DirtyDescriptions_test_final_corpus, 
                                               control =
                                              list(removePunctuation=TRUE,
                                              stopwords=TRUE,
                                              weighting = weightBin))

X_test_final <- as.matrix(cleandrug_test_final_dtm)
normalized_mean<-(sum(X_test_final)-mean(trainRowSums))/sd(trainRowSums)
normalized<-(sum(X_test_final)-min(trainRowSums))/(max(trainRowSums)-min(trainRowSums))

svm.predtrain_1<-predict(svm.model,normalized_mean)
(0-mean(trainRowSums))/sd(trainRowSums)

svm.predtrain_1<-predict(svm.model,-1)

predict(svm.model,-1)

DirtyDescriptions=MyData$Description
#I need a corpus
DirtyDescriptions_source <- VectorSource(DirtyDescriptions)
DirtyDescriptions_corpus <- VCorpus(DirtyDescriptions_source)
#A clean corpus
cleandrugs_corp <- clean_corpus(DirtyDescriptions_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrug_dtm <- DocumentTermMatrix(cleandrugs_corp)
#I hate useless words
cleandrug_dtm <- removeSparseTerms(cleandrug_dtm, 0.995)

x_train_get_number_of_words<-as.matrix(cleandrug_dtm)
x_train_get_number_of_words<-as.data.frame(x_train_get_number_of_words)

mean_number_of_words=mean(rowSums(x_train_get_number_of_words))








#confusionMatrix(confTest,positive='TRUE')
write.csv(file="NaturalPredictors.txt",NaturalPredictors)




summary(rowSums(testpredictors[which(as.logical(svm.predtest*(!(testLabels)))),]))


print(confTrain)
print(confTest)

summary(testpredictors[, colSums(testpredictors != 0) > 0])










svm.model.2<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.2, scale=TRUE, kernel="radial")

svm.predtrain.2<-predict(svm.model.2,trainpredictors)
svm.predtest.2<-predict(svm.model.2,testpredictors)

confTrain.2<-table(Predicted=svm.predtrain.2,Reference=trainLabels)
confTest.2<-table(Predicted=svm.predtest.2,Reference=testLabels)


print(confTrain.2)
print(confTest.2)


svm.model.3<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.05, scale=TRUE, kernel="radial")

svm.predtrain.3<-predict(svm.model.3,trainpredictors)
svm.predtest.3<-predict(svm.model.3,testpredictors)

confTrain.3<-table(Predicted=svm.predtrain.3,Reference=trainLabels)
confTest.3<-table(Predicted=svm.predtest.3,Reference=testLabels)


svm.model.4<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.6, scale=TRUE, kernel="radial")

svm.predtrain.4<-predict(svm.model.4,trainpredictors)
svm.predtest.4<-predict(svm.model.4,testpredictors)

confTrain.4<-table(Predicted=svm.predtrain.4,Reference=trainLabels)
confTest.4<-table(Predicted=svm.predtest.4,Reference=testLabels)



svm.model.5<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.4, scale=TRUE, kernel="radial")

svm.predtrain.5<-predict(svm.model.5,trainpredictors)
svm.predtest.5<-predict(svm.model.5,testpredictors)

confTrain.5<-table(Predicted=svm.predtrain.5,Reference=trainLabels)
confTest.5<-table(Predicted=svm.predtest.5,Reference=testLabels)



svm.model.6<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.35, scale=TRUE, kernel="radial")

svm.predtrain.6<-predict(svm.model.6,trainpredictors)
svm.predtest.6<-predict(svm.model.6,testpredictors)

confTrain.6<-table(Predicted=svm.predtrain.6,Reference=trainLabels)
confTest.6<-table(Predicted=svm.predtest.6,Reference=testLabels)



svm.model.7<-svm(trainpredictors,y=NULL, type='one-classification', nu=0.05, scale=TRUE, kernel="linear")

svm.predtrain.7<-predict(svm.model.7,trainpredictors)
svm.predtest.7<-predict(svm.model.7,testpredictors)

confTrain.7<-table(Predicted=svm.predtrain.7,Reference=trainLabels)
confTest.7<-table(Predicted=svm.predtest.7,Reference=testLabels)






print(confTrain)
print(confTest)

print(confTrain.2)
print(confTest.2)

print(confTrain.3)
print(confTest.3)

print(confTrain.4)
print(confTest.4)

print(confTrain.5)
print(confTest.5)

print(confTrain.6)
print(confTest.6)

print(confTrain.7)
print(confTest.7)
