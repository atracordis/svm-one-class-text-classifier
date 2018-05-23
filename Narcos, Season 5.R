source("Functions2.R")

AllDrugsCategory <- read.table("Category_New.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
AllDrugsDescriptions <- read.table("Description_New.csv",stringsAsFactors = FALSE, header=T,fill=T, encoding="utf-8", sep="²", dec=".",quote = "", na.string="" )

colors <- c('rgb(211,94,96)', 'rgb(114,147,203)')
library(plotly)
cat=as.data.frame(table(AllDrugsCategory))
cat=cat[order(cat$Freq, decreasing = T),]
cat

p<-  plot_ly(labels = ~head(cat$AllDrugsCategory, 10), values = ~head(cat$Freq, 10)) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donut charts using Plotly",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


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
AllDrugs$Category=droplevels(AllDrugs$Category)



NonDrugs <- read.table("flipkart.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
names(NonDrugs)=c("Description")
NonDrugs$Description=as.character(NonDrugs$Description)
NonDrugs=NonDrugs[which(nchar (NonDrugs$Description)>180),]


Drugs=AllDrugs$Description
Drugs=as.character(Drugs)
NonDrugs=NonDrugs
NonDrugs=as.character(NonDrugs)

LegalVector<- vector(mode="numeric", length=19962)
IllegalVector<- vector(mode="numeric", length=19579)
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

findFreqTerms(cleandrug_dtm, 10)
freq = data.frame(sort(colSums(as.matrix(cleandrug_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


summary(cleandrugs_corp)
#change it to a Boolean matrix
cleandrug_tdm <- removeSparseTerms(cleandrug_tdm, 0.98)
freq
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
write.csv(colnames(X), file = "names.csv")

write.svm(sv, svm.file = "Drug-classifier.svm", scale.file = "Drug-classifier.scale")






library(caret)
ctrl <- trainControl(method = "repeatedcv", repeats = 3,savePred=T, classProb=T)
mod <- train(y~., data=train, method = "svmLinear", trControl = ctrl)
