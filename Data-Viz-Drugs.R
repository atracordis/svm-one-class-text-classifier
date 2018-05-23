



DirtyDescriptionsDrugs_source <- VectorSource(Drugs)
DirtyDescriptionsDrugs_corpus <- VCorpus(DirtyDescriptionsDrugs_source)
#A clean corpus
cleandrugs_corp_Drugs <- clean_corpus(DirtyDescriptionsDrugs_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrugdrugs_dtm <- DocumentTermMatrix(cleandrugs_corp_Drugs, control = list(weighting = weightTfIdf))
#I hate useless words
cleandrugdrugs_dtm <- removeSparseTerms(cleandrugdrugs_dtm, 0.95)

cleandrugdrugs_tdm <- TermDocumentMatrix(cleandrugs_corp_Drugs, control = list(weighting = weightTfIdf))

cleandrugdrugs_tdm <- removeSparseTerms(cleandrugdrugs_tdm, 0.98)

findFreqTerms(cleandrugdrugs_dtm, 10)
freq_drugs = data.frame(sort(colSums(as.matrix(cleandrugdrugs_dtm)), decreasing=TRUE))
wordcloud(rownames(freq_drugs), freq_drugs[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


vizdrugsdrugs=as.matrix(cleandrugdrugs_tdm)

vizdrugsdrugs[vizdrugsdrugs>=1] <- 1
# transform into a term-term adjacency matrix
vizdrugsdrugs <- vizdrugsdrugs %*% t(vizdrugsdrugs)
# inspect terms numbered 5 to 10
somewords=vizdrugsdrugs[1:10,1:10]

# build a graph from the above matrix
g_drugs <- graph.adjacency(somewords, weighted=T, mode = "undirected")
# remove loops
g_drugs <- simplify(g_drugs)
# set labels and degrees of vertices
V(g_drugs)$label <- V(g_drugs)$name
V(g_drugs)$degree <- degree(g_drugs)
V(g_drugs)$label.cex <- 5 * V(g_drugs)$degree / max(V(g_drugs)$degree)+ .2
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g_drugs)
tkplot(g_drugs, layout=layout1)


# creating of document matrix
options(mc.cores=1)
drugs_dtm_drugs.2g <- DocumentTermMatrix(cleandrugs_corp_Drugs, control = list(weighting = weightTfIdf, tokenize = BigramTokenizer))
options(mc.cores=1)
drugs_dtm_drugs.3g <- DocumentTermMatrix(cleandrugs_corp_Drugs, control = list(weighting = weightTfIdf, tokenize = ThreegramTokenizer))
options(mc.cores=1)
drugs_dtm_drugs.4g <- DocumentTermMatrix(cleandrugs_corp_Drugs, control = list(weighting = weightTfIdf, tokenize = FourgramTokenizer))

inspect(drugs_dtm_drugs.2g)
# To get the bigram dist, we use the slam package for ops with simple triplet mat
sums_drugs.2g <- colapply_simple_triplet_matrix(drugs_dtm_drugs.2g,FUN=sum)
sums_drugs.2g <- sort(sums_drugs.2g, decreasing=T)

sums_drugs.3g <- colapply_simple_triplet_matrix(drugs_dtm_drugs.3g,FUN=sum)
sums_drugs.3g <- sort(sums_drugs.3g, decreasing=T)

sums_drugs.4g <- colapply_simple_triplet_matrix(drugs_dtm_drugs.4g,FUN=sum)
sums_drugs.4g <- sort(sums_drugs.4g, decreasing=T)

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.


barplot(head(sums_drugs.2g,10), main="Number of ideal cars per car retailer", horiz=TRUE,
       names.arg=head(colnames(sums_drugs.2g),10),  cex.names=0.8)

barplot(head(sums_drugs.3g,10), main="Number of ideal cars per car retailer", horiz=TRUE,
       names.arg=head(colnames(sums_drugs.3g),10),  cex.names=0.8)

barplot(head(sums_drugs.4g,10), main="Number of ideal cars per car retailer", horiz=TRUE,
       names.arg=head(colnames(sums_drugs.4g),10),  cex.names=0.8)

library(plotly)
par(las=2) # make label text perpendicular to axis
par(mar=c(20,20,20,20)) # increase y-axis margin.

p1 <- plot_ly(x = head(sums_drugs.4g,10), y = head(names(sums_drugs.4g),10), type = 'bar', orientation='v')
p1

p <- plot_ly(
  x = head(names(sums_drugs.4g),10),
  y = head(sums_drugs.4g,10),
  name = "SF Zoo",
  type = "bar"
)
p
p <- plot_ly(x = c(20, 14, 23), y = c('giraffes', 'orangutans', 'monkeys'), type = 'bar', orientation = 'h')
p

