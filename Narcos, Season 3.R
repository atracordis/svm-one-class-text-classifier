source("Narcos, Season 2.R")

#Document Term Matrix, because we need to be organized folk
cleandrug_dtm <- DocumentTermMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf))
#I hate useless words
cleandrug_dtm <- removeSparseTerms(cleandrug_dtm, 0.9)
#cleandrug_dtm <- removeSparseTerms(cleandrug_dtm, 0.95)
#Frequent terms for a word cloud. Build me a castle of words!
findFreqTerms(cleandrug_dtm, 10)
freq = data.frame(sort(colSums(as.matrix(cleandrug_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

target=AllDrugs$IsLegal
y=as.numeric(target)
y=as.factor(y)
X <- as.matrix(cleandrug_dtm)
sv <- svm(y~., train, type="C-classification", kernel="radial", cost=1, gamma=0.5)
table(Pred=predict(sv, test[,-1]) , True=test$y)
table(Pred=predict(sv, train[,-1]) , True=train$y)





write.svm(sv, svm.file = "drugs-classifier.svm", scale.file = "drugs-classifier.scale")
colnames(x)
write.csv(colnames(x),"words.csv")
data <- as.data.frame(cbind(y,X))

train.index <- sample(1:length(target), size=floor(.8*length(target)), replace=FALSE) 
train <- data[train.index,]
test <- data[-train.index,]

DirtyDescriptions=c("heroine cocaine iupac free", "shipping nylon zolpidem", "zip zipflyseparating zipped")
#I need a corpus
DirtyDescriptions_source <- VectorSource(DirtyDescriptions)
DirtyDescriptions_corpus <- VCorpus(DirtyDescriptions_source)
#A clean corpus
cleandrugs_corp <- clean_corpus(DirtyDescriptions_corpus)
#Document Term Matrix, because we need to be organized folk
cleandrug_dtm <- DocumentTermMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf))
#I hate useless words

# fit the svm and do a simple validation test. Cost parameter should be tuned.
#model <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:10), gamma=c(.01,10,2)))
#model
#cleandrug_m <- as.matrix(cleandrug_dtm_rm_sparse)

target=c("1","2","2")


y=as.numeric(target)
y=as.factor(y)
X <- as.matrix(cleandrug_dtm)
data
data <- as.data.frame(cbind(y,X))
predict(sv, x)
table(Pred=predict(sv, data[,-1]) , True=data$y)


