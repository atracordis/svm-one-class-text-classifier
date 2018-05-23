

IsThisLegal=MyData$Legality
IsThisLegal=as.numeric(target)
IsThisLegal=as.factor(IsThisLegal)

X <- as.matrix(cleandrug_dtm)
data <- as.data.frame(cbind(IsThisLegal,X))
data$IsThisIllegal=data$IsThisLegal==0
str(data)

for(b in colnames(data)) data[[b]] <- as.logical(data[[b]])
trans <- as(data, "transactions")

basket_rules<-apriori(data,parameter=list(sup=0.01,conf=0.6,target="rules"))

inspect(basket_rules)



w= subset( basket_rules, subset = ( (lhs %pin% "IsThisIllegal" )  | ( rhs %pin% "IsThisIllegal" ) ))  

View(as(w,"data.frame"))
w=as(w,"data.frame")

wrules= data.frame(w$rules, w$lift)
wrules=wrules[order(wrules$w.lift, decreasing=T),]

lift=wrules$w.lift
rules=wrules$w.rules
par(las=2) # make label text perpendicular to axis
par(mar=c(5,16,4,2)) # increase y-axis margin.
barplot(head(lift,10), main="Most critical rules", horiz=TRUE,
        names.arg=head(rules,10),  cex.names=0.8)

