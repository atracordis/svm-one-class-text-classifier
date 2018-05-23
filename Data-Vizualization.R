

library(SnowballC)
library(class)
library(wordcloud)
findFreqTerms(cleandrug_dtm, 10)
freq = data.frame(sort(colSums(as.matrix(cleandrug_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


vizdrugs=as.matrix(cleandrug_tdm)

vizdrugs[vizdrugs>=1] <- 1
# transform into a term-term adjacency matrix
vizdrugs <- vizdrugs %*% t(vizdrugs)
# inspect terms numbered 5 to 10
vizdrugs[5:10,5:10]

library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(vizdrugs, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

set.seed(3952)


plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)



library(RWeka)
library(slam)
sums <- colapply_simple_triplet_matrix(cleandrug_dtm,FUN=sum)
sums <- sort(sums, decreasing=T)


BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=2, max=2))}
ThreegramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=3, max=3))}
FourgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min=4, max=4))}


# creating of document matrix
options(mc.cores=1)
drugs_dtm.2g <- DocumentTermMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf, tokenize = BigramTokenizer))
options(mc.cores=1)
drugs_dtm.3g <- DocumentTermMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf, tokenize = ThreegramTokenizer))
options(mc.cores=1)
drugs_dtm.4g <- DocumentTermMatrix(cleandrugs_corp, control = list(weighting = weightTfIdf, tokenize = FourgramTokenizer))


drugs_tdm.2g <- removeSparseTerms(drugs_tdm.2g, 0.9)

inspect(drugs_tdm.2g)
# To get the bigram dist, we use the slam package for ops with simple triplet mat
sums.2g <- colapply_simple_triplet_matrix(drugs_dtm.2g,FUN=sum)
sums.2g <- sort(sums.2g, decreasing=T)

sums.3g <- colapply_simple_triplet_matrix(drugs_dtm.3g,FUN=sum)
sums.3g <- sort(sums.3g, decreasing=T)

sums.4g <- colapply_simple_triplet_matrix(drugs_dtm.4g,FUN=sum)
sums.4g <- sort(sums.4g, decreasing=T)
 

barplot(head(sums.2g))
library(ggplot2)



barplot(head(sums.3g))

barplot(head(sums.4g))
