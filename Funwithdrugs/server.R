#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tm)
library(e1071)
library(qdap)
library(SnowballC)
load(file="Everything.RData")

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  corpus <- tm_map(corpus, toSpace, "[^a-zA-Z ]")
  corpus <- tm_map(corpus, toSpace, "â")
  corpus <- tm_map(corpus, toSpace, "ª")
  corpus <- tm_map(corpus, toSpace, "œ")
  corpus <- tm_map(corpus, toSpace, "¤")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("the","day","yes","yet", "like","œ","¤","â","will", "sale","get", "good","also","best","can",  "and", stopwords("english") ))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, removeWords, c("dri","line","busi","die","extrem","china","oil","Durban","green","classic","citrus","crush","cheap","old","oral","trust","user","use","mtehod","suppli","lemon","extra","usp","vacuum","trim","straight","satisfact","usd","Dri","Green","Addit","youll","your","collect","huge","flipkartcom","price","order","list","onlin","pleas","ship","buy","com","flipkart","aaa","deliveri","cash","one","india","shop","read","set","print","new","page","come","requir","give","name","know","item","general","red","question","add","final","take","want","see","around","discount","content","two"))
  
  corpus <- tm_map(corpus, removeWords, c("addit", "amnesia", "aroma", "bar", 
                                          "batch","display", "black", "blister", "blueberri",
                                          "buzz","blend","crystal", "contact","chemic", "chine", "clean", "clear", "cure",
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

clean_normalize <- function(AllPredictors,drug_string){
  stemmed_drug_string<-stemmer(drug_string, warn=F)
  stemmed_drug_string<-tolower(stemmed_drug_string) 
  stemmed_drug_string<- stemmed_drug_string[stemmed_drug_string %in% AllPredictors]
  stemmed_drug_string<-paste(stemmed_drug_string,collapse=" ")
  DirtyDescriptions_test_final_source <- VectorSource(stemmed_drug_string)
  DirtyDescriptions_test_final_corpus <- VCorpus(DirtyDescriptions_test_final_source)
  cleandrugs_test_final_corp <- clean_corpus(DirtyDescriptions_test_final_corpus)
  cleandrug_test_final_dtm <- DocumentTermMatrix(DirtyDescriptions_test_final_corpus,control =list(weighting = weightBin))
  X_test_final <- as.matrix(cleandrug_test_final_dtm)
  return(min(ifelse(sum(X_test_final)==1,AllYouNeed['Mean'],sum(X_test_final) ), AllYouNeed['Mean']));
}

library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {

    output$summary <- renderText({
    input$goButton
    isolate(
      ifelse(predict(svm.model, clean_normalize(AllPredictors,input$text1)),
      "This is a drug","This is not a drug"))
  })
  
  
})
