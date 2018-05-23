library(stringr)
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
library(mongolite)
library(RMongo)
library(jsonlite)

Mongo_descriptions = mongo(collection = "descriptions", db = "drugs") # create connection, database and collection
Mongo_categories = mongo(collection = "categories", db = "drugs") # create connection, database and collection
Mongo_nondrugdescriptions = mongo(collection = "descriptions", db = "nondrugs") # create connection, database and collection

Mongo_descriptions$drop()
Mongo_categories$drop()
Mongo_nondrugdescriptions$drop()

AllDrugsCategory <- read.table("Category_New.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
AllDrugsDescriptions <- read.table("Description_New.csv",stringsAsFactors = FALSE, header=T,fill=T, encoding="utf-8", sep="²", dec=".",quote = "", na.string="" )
NonDrugs <- read.table("flipkart.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )

Mongo_descriptions$insert(AllDrugsDescriptions)
Mongo_categories$insert(AllDrugsCategory)
Mongo_nondrugdescriptions$insert(NonDrugs)


AllDrugsDescriptions=Mongo_descriptions$find()
AllDrugsCategory=Mongo_categories$find()
NonDrugs=Mongo_nondrugdescriptions$find()

BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=2, max=2))}
ThreegramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=3, max=3))}
FourgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=4, max=4))}
 
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  corpus <- tm_map(corpus, toSpace, "[^a-zA-Z ]")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("the","day","yes","yet", "like","œ","¤","â","will", "sale","get", "good","also","best","can",  "and", stopwords("english") ))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, removeWords, c("product","offer","qualiti","usd","collect","huge","flipkartcom","price","order","list","onlin","pleas","ship","buy","com","flipkart","deliveri","cash","one","india","shop","read","set","print","new","page","come","requir","give","name","know","item","general","red","question","add","final","take","want","see","around","discount","content","two", "time","fe", "lostheaven","time", "use", "great", "necklac","rs" ,"alloy","top", "updat", "ab", "everi"))
  return(corpus)
}

 