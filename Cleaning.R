source("Parse.R")
source.utf8("Functions.R")

AllDrugs <- read.table("DrugsFlipkart.csv", header=T,fill=T, encoding="utf-8", sep="|", dec=".",quote = "", na.string="" )
names(AllDrugs)=c("Desc","IsLegal", "Title")
AllDrugs$Title <- as.character(AllDrugs$Title)
AllDrugs$Title <- ifelse(is.na(AllDrugs$Title), ' ', AllDrugs$Title)


AllDrugs$Desc=as.character(AllDrugs$Desc)
AllDrugs$Desc<- paste(AllDrugs$Title, sep=" ", AllDrugs$Desc)
any(is.na(AllDrugs$Desc ))

AllDrugs<-subset(AllDrugs,!(is.na(AllDrugs["IsLegal"])       ))
str(AllDrugs$Desc)



DirtyDescriptions=AllDrugs$Desc
DirtyDescriptions_source <- VectorSource(DirtyDescriptions)
DirtyDescriptions_corpus <- VCorpus(DirtyDescriptions_source)
cleandrugs_corp <- clean_corpus(DirtyDescriptions_corpus)


cleandrug_dtm <- DocumentTermMatrix(cleandrugs_corp)
cleandrug_dtm_rm_sparse <- removeSparseTerms(cleandrug_dtm, 0.99)
cleandrug_m <- as.matrix(cleandrug_dtm_rm_sparse)

findFreqTerms(cleandrug_dtm_rm_sparse, 1000)



Weighteddtm <- weightTfIdf(cleandrug_dtm_rm_sparse,normalize=TRUE)
mat.df <- as.data.frame(data.matrix(Weighteddtm), stringsAsfactors = FALSE)
mat.df <- cbind(mat.df, AllDrugs$IsLegal)
colnames(mat.df)[ncol(mat.df)] <- "Class"
Assignment.Distribution <- table(mat.df$Class)

Res_Desc_Train_Assign <- mat.df$Class

Assignment.Distribution <- table(mat.df$Class)

### Feature has different ranges, normalizing to bring ranges from 0 to 1
### Another way to standardize using z-scores

#normalize(c(1,2,3,4,5))

num_col <- ncol(mat.df)-1
mat.df_normalize <- as.data.frame(lapply(mat.df[,1:num_col], normalize))
mat.df_normalize <- cbind(mat.df_normalize, Res_Desc_Train_Assign)
colnames(mat.df_normalize)[ncol(mat.df_normalize)] <- "Class"

#names(mat.df)
outcomeName <- "Class"

train = mat.df_normalize[c(1:nrow(AllDrugs)*0.8),]
test = mat.df_normalize[((nrow(AllDrugs)*0.8+1):nrow(AllDrugs)),]


train$Class <- as.factor(train$Class) 

###SVM Model
x <- subset(train, select = -Class)
y <- train$Class



model <- svm(x, y, probability = TRUE) 
test1 <- subset(test, select = -Class)
model <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
model
#select values of cost & gamma from here and pass it to tuned_model
tuned_model <- svm(x, y, kernel="radial", cost=1, gamma=0.5,probability = TRUE)
svm.pred <- predict(tuned_model, test1, decision.values = TRUE, probability = TRUE)
svm_prob <- attr(svm.pred, "probabilities")


finalresult <- cbind(test,svm.pred,svm_prob)
str(finalresult)
finalresult



