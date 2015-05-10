#
## step 1 to explore the data and work with outliers or null fields
##
adultD <- read.table("data/adult.data", head=FALSE, sep=",")
adultDT <- read.table("data/adult.test", head=FALSE, sep=",")
adult<-rbind(adultD, adultDT)
head(adult)
#set the variable names.
columns <- c("age", "workclass","fnlwgt","education","education-num","martial-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","income")
colnames(adult) <- columns 

summary(adult)

#capital gain has errors?
table(adult['capital-gain']==99999) # 159 equal to 99999 none bigger.
# this could be  the biggest number in the census form.


par(mfrow=c(1,1), las=1)
?boxplot
boxplot(adult$"capital-gain", xlab="capital gain")
boxplot(adult$"capital-loss", xlab="capital loss")
boxplot(adult$"hours-per-week", xlab="hours per week")
attach(adultD)# to work with the data without having to access the dF
summary(ocupation)
summary(workclass)
subset(adultD, workclass="?"  )
adultD$workclass=="?"
?subset
