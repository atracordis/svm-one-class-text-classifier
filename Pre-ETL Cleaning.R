library(dplyr)
AllDrugsCategory <- read.table("Category_New.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
AllDrugsDescriptions <- read.table("Description_New.csv",stringsAsFactors = FALSE, header=T,fill=T, encoding="utf-8", sep="²", dec=".",quote = "", na.string="" )


AllDrugsDescriptions<-AllDrugsDescriptions[1:109690,]
AllDrugsDescriptions<-AllDrugsDescriptions$Item.Description.sep
AllDrugsCategory<-AllDrugsCategory$Category
  
AllDrugs<-data.frame(AllDrugsDescriptions, AllDrugsCategory)
names(AllDrugs)=c("Description", "Category")
AllDrugs$Description=as.character(AllDrugs$Description)
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Cannabis/W"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Dissociatives/PCP"),]
AllDrugs=AllDrugs[which(AllDrugs$Category!="Drugs/Barbiturates"),]

 
AllDrugs=AllDrugs[which(substr(AllDrugs$Category,1,6)=="Drugs/"),]


AllDrugs=AllDrugs[which(nchar (AllDrugs$Description)>200),]
AllDrugs$Category=droplevels(AllDrugs$Category)
table(AllDrugs$Category)
new_df <- AllDrugs %>% group_by(Category) %>% sample_n(12)




NonDrugs <- read.table("SampleData.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
Drugs=new_df$Description
Drugs=as.character(Drugs)
NonDrugs=NonDrugs$description.
NonDrugs=as.character(NonDrugs)

LegalVector<- vector(mode="numeric", length=500)
IllegalVector<- vector(mode="numeric", length=588)
IllegalVector=rep(0,588)
LegalVector=rep(1,500)
Everything=c(Drugs,NonDrugs)
LegalIllegal=c(IllegalVector,LegalVector)

MyData=data.frame(Everything,LegalIllegal)
names(MyData)=c("Description", "Legality")



