library(stringr)
library(tm)
library(qdap)
library(e1071)
library(SnowballC)
library(wordcloud)
library(RTextTools)
library(tidytext)
library(dplyr)

normalize <- function(x) {
  y <- min(x)
  z <- max(x)
  temp <- x - y
  temp1 <- (z - y)
  temp2 <- temp / temp1
  return(temp2)
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
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  #URLs
 # urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
  # corpus<-tm_map(corpus, content_transformer(urlPat))
  # Emails
  #  emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
  #corpus<- tm_map(corpus, content_transformer(emlPat))
  # Usernames
  #tun<-function(x) gsub("[a - zA - Z0 - 9_]{1,15}", "", x)
  #corpus<- tm_map(corpus,content_transformer(tun))
  
#  tun2<-function(x) gsub("[^a-z]", "", x)
 # corpus<- tm_map(corpus,content_transformer(tun2))

 toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, toSpace, "[^a-zA-Z ]")
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  corpus <- tm_map(corpus, toSpace, "â")
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  corpus <- tm_map(corpus, toSpace, "ª")
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  corpus <- tm_map(corpus, toSpace, "œ")
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  corpus <- tm_map(corpus, toSpace, "¤")
  
  
  weird="âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœªâ¤âœª"
  #Remove profane words
  #First get the list of bad words
  #bwdat<-read.table("en_bws.txt", header=FALSE, sep="\n", strip.white=TRUE)
 # names(bwdat)<-"Bad Words"
#  corpus<- tm_map(corpus, removeWords, bwdat[,1])

  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("the", "like","œ","¤","â","will", "sale","get", "good","also","best","can",  weird,  "and", stopwords("english") ))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stemDocument, language = "english")
  
  return(corpus)
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
