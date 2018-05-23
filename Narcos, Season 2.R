source("Parse.R")
source("Pre-ETL Cleaning.R")
source.utf8("Functions.R")
#Loading Data
#AllDrugs <- read.table("DrugsFlipkart.csv", header=T,fill=T, encoding="utf-8", sep="\t", dec=".",quote = "", na.string="" )
Descriptions<-read.table("description.csv", header=T, fill=T, sep="\n", dec=".", quote="", na.string="")
Titles<-read.table("Title.csv", header=T, fill=T, sep="\n", dec=".", quote="")  
IsLegal<-read.table("IsLegal.csv", header=T, fill=T, sep="\n", dec=".", quote="", na.string="")
AllDrugs <- data.frame(Descriptions, IsLegal, Titles)


#Giving new names to the columns. I hate the old ones
names(AllDrugs)=c("Desc","IsLegal", "Title")
summary ( as.factor(AllDrugs$IsLegal))
#I don't like nas.
AllDrugs$Title <- as.character(AllDrugs$Title)
AllDrugs$Title <- ifelse(is.na(AllDrugs$Title), ' ', AllDrugs$Title)
#We're gonna need the title and the description concatenated,
#because sometimes, the description is empty. Damn drug dealers!
AllDrugs$Desc=as.character(AllDrugs$Desc)
AllDrugs$Desc<- paste(AllDrugs$Title, sep=" ", AllDrugs$Desc)
#Checking if na
any(is.na(AllDrugs$Desc ))
#All cool.
AllDrugs<-subset(AllDrugs,!(is.na(AllDrugs["IsLegal"])))
#Oh and by the way, I still hate na's.
str(AllDrugs$Desc)
#Seems good to me
#Give me the dirt now.
DirtyDescriptions=AllDrugs$Desc
#I need a corpus
DirtyDescriptions_source <- VectorSource(DirtyDescriptions)
DirtyDescriptions_corpus <- VCorpus(DirtyDescriptions_source)
#A clean corpus
cleandrugs_corp <- clean_corpus(DirtyDescriptions_corpus)

