  
  DirtyDescriptionsDrugs_source <- VectorSource(Drugs)
  DirtyDescriptionsDrugs_corpus <- VCorpus(DirtyDescriptionsDrugs_source)
  #A clean corpus
  cleandrugs_corp_Drugs <- clean_corpus(DirtyDescriptionsDrugs_corpus)
  
  #Document Term Matrix, because we need to be organized folk
  cleandrugdrugs_dtm <- DocumentTermMatrix(cleandrugs_corp_Drugs, control = list(weighting = weightTfIdf))
  #I hate useless words
  cleandrugdrugs_dtm <- removeSparseTerms(cleandrugdrugs_dtm, 0.95)
  
  
  targetdrug=IllegalVector
  ydrug=as.numeric(targetdrug)
  ydrug=as.factor(ydrug)
  Xdrug <- as.matrix(cleandrugdrugs_dtm)
  #dataDrug <- as.data.frame(cbind(ydrug,Xdrug))
  dataDrug <- as.data.frame(Xdrug)
  
  str(dataDrug)
  for(b in colnames(dataDrug)) dataDrug[[b]] <- as.logical(dataDrug[[b]])
  transDrug <- as(dataDrug, "transactions")
  
  basket_rules_drug<-apriori(transDrug,parameter=list(sup=0.001,conf=0.6,target="rules"))
   
  wdrug=as(basket_rules_drug,"data.frame")
  wruleslife= data.frame(wdrug$rules, wdrug$lift)
  wruleslife=wruleslife[order(wruleslife$wdrug.lift, decreasing=T),]
  
  liftdrugs=wruleslife$wdrug.lift
  rulesdrugs=wruleslife$wdrug.rules
  par(las=2) # make label text perpendicular to axis
  par(mar=c(5,12,4,2)) # increase y-axis margin.
  barplot(head(liftdrugs,10), main="Most critical rules", horiz=TRUE,
          names.arg=head(rulesdrugs,10),  cex.names=0.8)
  library(arulesViz)
  plot(basket_rules_drug[1:20],
       
       method = "graph",
      interactive = TRUE,
       control = list(type = "items"))
  plotly_arules(basket_rules_drug[1:20], jitter = 10,method="scatterplot",
                marker = list(opacity = .7, size = 10, symbol = 1),
                colors = c("blue", "green"))
  