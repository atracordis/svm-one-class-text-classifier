library(mongolite)
library(RMongo)
library(jsonlite)

Mongo_descriptions = mongo(collection = "descriptions", db = "drugs") # create connection, database and collection
Mongo_categories = mongo(collection = "categories", db = "drugs") # create connection, database and collection
Mongo_nondrugdescriptions = mongo(collection = "descriptions", db = "nondrugs") # create connection, database and collection

Mongo_descriptions$drop()
Mongo_categories$drop()
Mongo_nondrugdescriptions$drop()

AllDrugsCategory <- read.table("Category_New.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )
AllDrugsDescriptions <- read.table("Description_New.csv",stringsAsFactors = FALSE, header=T,fill=T, encoding="utf-8", sep="²", dec=".",quote = "", na.string="" )
NonDrugs <- read.table("flipkart.csv", header=T,fill=T, encoding="utf-8", sep="\n", dec=".",quote = "", na.string="" )

Mongo_descriptions$insert(AllDrugsDescriptions)
Mongo_categories$insert(AllDrugsCategory)
Mongo_nondrugdescriptions$insert(NonDrugs)


AllDrugsDescriptions=Mongo_descriptions$find()
AllDrugsCategory=Mongo_categories$find()
NonDrugs=Mongo_nondrugdescriptions$find()
