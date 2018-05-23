
DirtyDescriptionsNonDrugs_source <- VectorSource(NonDrugs)
DirtyDescriptionsNonDrugs_corpus <- VCorpus(DirtyDescriptionsNonDrugs_source)
#A clean corpus
 
DirtyDescriptionsNonDrugs_corpus <- clean_corpus(DirtyDescriptionsNonDrugs_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrugnondrugs_dtm <- DocumentTermMatrix(DirtyDescriptionsNonDrugs_corpus, control = list(weighting = weightTfIdf))
cleandrugnondrugs_dtm
#I hate useless words
cleandrugnondrugs_dtm <- removeSparseTerms(cleandrugnondrugs_dtm, 0.95)

targetnondrug=LegalVector
ynondrug=as.numeric(targetnondrug)
ynondrug=as.factor(ynondrug)
cleandrugnondrugs_dtm
Xnondrug <- as.matrix(cleandrugnondrugs_dtm)
datanonDrug <- as.data.frame(Xnondrug)

for(b in colnames(datanonDrug)) datanonDrug[[b]] <- as.logical(datanonDrug[[b]])
transnonDrug <- as(datanonDrug, "transactions")

basket_rules_nondrug<-apriori(transnonDrug,parameter=list(sup=0.15,conf=0.6,target="rules"))
View(as(basket_rules_nondrug,"data.frame"))

wnodrug=as(basket_rules_nondrug,"data.frame")
wruleslife= data.frame(wdrug$rules, wdrug$lift)
wruleslife=wruleslife[order(wruleslife$wdrug.lift, decreasing=T),]

liftdrugs=wruleslife$wdrug.lift
rulesdrugs=wruleslife$wdrug.rules
par(las=2) # make label text perpendicular to axis
par(mar=c(5,12,4,2)) # increase y-axis margin.
barplot(head(liftdrugs,10), main="Most critical rules", horiz=TRUE,
        names.arg=head(rulesdrugs,10),  cex.names=0.8)

