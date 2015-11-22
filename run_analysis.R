#Download and Unzip Data

setwd("c:/gettingandcleaningdata")
library(XML)
url<-c("http://archive.ics.uci.edu/ml/machine-learning-databases/00240/")
doc<-htmlParse(url)
#get <a> nodes.
Anodes<-getNodeSet(doc,"//a")
#get the ones with .zip's and .gz's
files<-grep("*.gz|*.zip",sapply(Anodes, function(Anode) xmlGetAttr(Anode,"href")),value=TRUE)
#make the full url
urls<-paste(url,files,sep="")
#Download each file.
mapply(function(x,y) download.file(x,y),urls,files)
unzip(files, junkpaths=TRUE, exdir="data")

#read in needed tables
training = read.csv("data/X_train.txt", sep="", header=FALSE)
#add in extra columns to bring in other data for training
training[,562] = read.csv("data/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("data/subject_train.txt", sep="", header=FALSE)

testing = read.csv("data/X_test.txt", sep="", header=FALSE)
#add in extra columns to bring in other data for testing (561 columns in large data set)
testing[,562] = read.csv("data/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("data/subject_test.txt", sep="", header=FALSE)

#bring in the activity labels
activityLabels = read.csv("data/activity_labels.txt", sep="", header=FALSE)

#Set foundation for descriptive activity names, match pattern for requested variables
features = read.csv("data/features.txt", sep="", header=FALSE)
features[,2] = gsub('-mean', 'Mean', features[,2])
features[,2] = gsub('-std', 'Std', features[,2])
features[,2] = gsub('[-()]', '', features[,2])

# Merge training and test sets together
everyThing = rbind(training, testing)

# Capture Mean and Stdev
dataIWant <- grep(".*Mean.*|.*Std.*", features[,2])
# First select the data I want
features <- features[dataIWant,]
# Now add the last two columns (subject and activity)
dataIWant <- c(dataIWant, 562, 563)
# And remove the unwanted columns from everyThing 
everyThing <- everyThing[,dataIWant ]
# Add the column names (features) to everyThing 
colnames(everyThing ) <- c(features$V2, "Activity", "Subject")
colnames(everyThing ) <- tolower(colnames(everyThing ))

#now loop through data and select the one's I want
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  everyThing$activity <- gsub(currentActivity, currentActivityLabel, everyThing$activity)
  currentActivity <- currentActivity + 1
}

everyThing$activity <- as.factor(allData$activity)
everyThing$subject <- as.factor(allData$subject)

#generate tidy table as required
tidy = aggregate(everyThing, by=list(activity = everyThing$activity, subject=everyThing$subject), mean)
# Remove the subject and activity column, since a mean of those has no use
tidy[,90] = NULL
tidy[,89] = NULL
write.table(tidy, "tidy.txt", sep="\t")
