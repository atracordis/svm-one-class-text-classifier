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

normalize <- function(x) {
  y <- min(x)
  z <- max(x)
  temp <- x - y
  temp1 <- (z - y)
  temp2 <- temp / temp1
  return(temp2)
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
  corpus <- tm_map(corpus, removeWords, c("usd","collect","huge","flipkartcom","price","order","list","onlin","pleas","ship","buy","com","flipkart","deliveri","cash","one","india","shop","read","set","print","new","page","come","requir","give","name","know","item","general","red","question","add","final","take","want","see","around","discount","content","two"))
  
  return(corpus)
}


Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}
Clean_Text_Block <- function(text){
  if(length(text) <= 1){
    # Check to see if there is any text at all with another conditional
    if(length(text) == 0){
      cat("There was no text in this document! \n")
      to_return <- list(num_tokens = 0, unique_tokens = 0, text = "")
    }else{
      # If there is , and only only one line of text then tokenize it
      clean_text <- Clean_String(text)
      num_tok <- length(clean_text)
      num_uniq <- length(unique(clean_text))
      to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
    }
  }else{
    # Get rid of blank lines
    indexes <- which(text == "")
    if(length(indexes) > 0){
      text <- text[-indexes]
    }  
    # Loop through the lines in the text and use the append() function to 
    clean_text <- Clean_String(text[1])
    for(i in 2:length(text)){
      # add them to a vector 
      clean_text <- append(clean_text,Clean_String(text[i]))
    }
    # Calculate the number of tokens and unique tokens and return them in a 
    # named list object.
    num_tok <- length(clean_text)
    num_uniq <- length(unique(clean_text))
    to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
  }
  return(to_return)
}

