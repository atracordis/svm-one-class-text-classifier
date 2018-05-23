DirtyDescriptionsNonDrugs_source <- VectorSource(NonDrugs)
DirtyDescriptionsNonDrugs_corpus <- VCorpus(DirtyDescriptionsNonDrugs_source)
#A clean corpus
DirtyDescriptionsNonDrugs_corpus <- clean_corpus(DirtyDescriptionsNonDrugs_corpus)

#Document Term Matrix, because we need to be organized folk
cleandrugnondrugs_dtm <- DocumentTermMatrix(DirtyDescriptionsNonDrugs_corpus, control = list(weighting = weightTfIdf))
#I hate useless words
cleandrugnondrugs_dtm <- removeSparseTerms(cleandrugnondrugs_dtm, 0.95)


#On prépare et nettoie la term document matrix


cleandrugnondrugs_tdm <- TermDocumentMatrix(DirtyDescriptionsNonDrugs_corpus, control = list(weighting = weightTfIdf))
cleandrugnondrugs_tdm <- removeSparseTerms(cleandrugnondrugs_tdm, 0.98)

#On commence par étudier les mots les plus fréquents


findFreqTerms(cleandrugnondrugs_tdm, 10)
freq_drugs = data.frame(sort(colSums(as.matrix(cleandrugnondrugs_dtm)), decreasing=TRUE))
wordcloud(rownames(freq_drugs), freq_drugs[,1], max.words=50, colors=brewer.pal(1, "Dark2"))


#on passe ensuite à la visualisation des mots qui viennent ensemble. On ne prendra que les mots qui existent


vizdrugsnondrugs=as.matrix(cleandrugnondrugs_tdm)
vizdrugsnondrugs[vizdrugsnondrugs>=1] <- 1


#On multiplie la matrice par sa transposée

# transform into a term-term adjacency matrix
vizdrugsnondrugs <- vizdrugsnondrugs %*% t(vizdrugsnondrugs)


#On prend le top dix des combinaisons les plus fréquentes

# inspect terms numbered 5 to 10
morewords=vizdrugsnondrugs[1:10,1:10]


#On va à présent visualiser un graphe contenant les mots qui viennent ensemble

# build a graph from the above matrix
g_nondrugs <- graph.adjacency(morewords, weighted=T, mode = "undirected")
# remove loops
g_nondrugs <- simplify(g_nondrugs)
# set labels and degrees of vertices
V(g_nondrugs)$label <- V(g_nondrugs)$name
V(g_nondrugs)$degree <- degree(g_nondrugs)
V(g_nondrugs)$label.cex <- 5 * V(g_nondrugs)$degree / max(V(g_nondrugs)$degree)+ .2
set.seed(3952)
layout2 <- layout.kamada.kawai(g_nondrugs)
plot(g_nondrugs, layout=layout2)

#on va créer les document term matrix tokenisés par bigram, trigram and fourgram, pour voir les séquences de mots les plus fréquentes

# creating of document matrix
options(mc.cores=1)
drugs_dtm_nondrugs.2g <- DocumentTermMatrix(DirtyDescriptionsNonDrugs_corpus, control = list(weighting = weightTfIdf, tokenize = BigramTokenizer))
options(mc.cores=1)
drugs_dtm_nondrugs.3g <- DocumentTermMatrix(DirtyDescriptionsNonDrugs_corpus, control = list(weighting = weightTfIdf, tokenize = ThreegramTokenizer))
options(mc.cores=1)
drugs_dtm_nondrugs.4g <- DocumentTermMatrix(DirtyDescriptionsNonDrugs_corpus, control = list(weighting = weightTfIdf, tokenize = FourgramTokenizer))


#On utilise slam pour pouvoir préparer la fréquence des successions des termes qui se répètent

# To get the bigram dist, we use the slam package for ops with simple triplet mat
sums_nondrugs.2g <- colapply_simple_triplet_matrix(drugs_dtm_nondrugs.2g,FUN=sum)
sums_nondrugs.2g <- sort(sums_nondrugs.2g, decreasing=T)

sums_nondrugs.3g <- colapply_simple_triplet_matrix(drugs_dtm_nondrugs.3g,FUN=sum)
sums_nondrugs.3g <- sort(sums_nondrugs.3g, decreasing=T)

sums_nondrugs.4g <- colapply_simple_triplet_matrix(drugs_dtm_nondrugs.4g,FUN=sum)
sums_nondrugs.4g <- sort(sums_nondrugs.4g, decreasing=T)


#On fait une petite manoeuvre pour éclaircir le futur graphe

#Fréquence bigram des non drogues

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(head(sums_nondrugs.2g,10), main="Frequency of bigrams", horiz=TRUE,
        names.arg=head(colnames(sums_nondrugs.2g),10),  cex.names=0.8)


#Fréquence trigram des non drogues

par(las=2) # make label text perpenicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(head(sums_nondrugs.3g,10), main="Frequency of trigrams", horiz=TRUE,
        names.arg=head(colnames(sums_nondrugs.3g),10),  cex.names=0.8)


#Fréquence fourgram des non drogues

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(head(sums_nondrugs.4g,10), main="Frequency of fourgrams", horiz=TRUE,
        names.arg=head(colnames(sums_nondrugs.4g),10),  cex.names=0.8)

