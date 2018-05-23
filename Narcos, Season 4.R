source("Functions.R")

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
table(AllDrugs$Category)
new_df <- AllDrugs %>% group_by(Category) %>% sample_n(12)




NonDrugs <- read.table("SampleData.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
Drugs=new_df$Description
Drugs=as.character(Drugs)
NonDrugs=NonDrugs$description.
NonDrugs=as.character(NonDrugs)

LegalVector<- vector(mode="numeric", length=500)
IllegalVector<- vector(mode="numeric", length=588)
IllegalVector=rep(0,588)
LegalVector=rep(1,500)
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
cleandrug_dtm <- removeSparseTerms(cleandrug_dtm, 0.99)

findFreqTerms(cleandrug_dtm, 10)
freq = data.frame(sort(colSums(as.matrix(cleandrug_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

target=MyData$Legality
y=as.numeric(target)
y=as.factor(y)
X <- as.matrix(cleandrug_dtm)
data <- as.data.frame(cbind(y,X))
train.index <- sample(1:length(target), size=floor(.8*length(target)), replace=FALSE) 
train <- data[train.index,]
test <- data[-train.index,]

#modelsvm <- tune(svm, train.x=X, train.y=y, kernel="radial", ranges=list(cost=10^(-2:2), gamma=2^(-2:2)))

#write.svm(model, svm.file = "drugs-classifier.svm", scale.file = "drugs-classifier.scale")


sv <- svm(y~., train, type="C-classification", kernel="linear", cost=1, gamma=0.2)
table(Pred=predict(sv, test[,-1]) , True=test$y)
table(Pred=predict(sv, train[,-1]) , True=train$y)


