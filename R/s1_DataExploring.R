#
## step 1 to explore the data and work with outliers or null fields
##
adultD <- read.table("data/adult.data", head=FALSE, sep=",")
adultDT <- read.table("data/adult.test", head=FALSE, sep=",")
levels(adultDT$V15) <- c(" <=50K"," >50K")
adult<-rbind(adultD, adultDT)# 48796 size
#set the variable names.
columns <- c("age", "workclass","fnlwgt","education","education.num","martial.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country","income")
colnames(adult) <- columns 
levels(workclass)
summary(adult)

summary ( subset(adult, ( adult$occupation==" ?"&adult$"native.country"==" ?"&adult$workclass==" ?" ) ) )
indexToDelete<-as.numeric( rownames( subset(adult, ( adult$occupation==" ?"&adult$"native.country"==" ?"&
                                                       adult$workclass==" ?" ) ) ) )
#DELETED THE ROWS WITH 3 VARIABLES NULL
adult<-adult[-indexToDelete,]

summary(adult)
#capital gain has errors?
table(adult['capital.gain']==99999) # 159 equal to 99999 none bigger.
# this could be  the biggest number in the census form.
boxplot(adult$"capital.gain")
nrow( subset(adult, adult$"capital.gain">99000 ))
summary(adult)


############ Create new category for no workclass
attach(adult)
adult.p1 <- adult[1:2]
adult.p2 <- adult[3:length(adult)]
var = 0
for( i in 1:length(workclass)
)
{
  #print(i)
  # very very very slow haha couldn't find another way :(
  if(workclass[i] ==" ?")  
    var[i] = TRUE
  else
    var[i] = FALSE
}
var <- as.factor(var)
var <- as.data.frame(var)
colnames(var) <- c("noWorkClass")
adult <- cbind(adult.p1,var,adult.p2)
###########################

adult[,c("age","fnlwgt", "education.num","hours.per.week")]
library(chemometrics)
adult.cont <- adult[,c("age","fnlwgt", "education.num","hours.per.week")]
hist(adult$"capital.gain")
hist(adult$"capital.loss")
mahal<-Moutlier(adult.cont, plot=TRUE)

?Moutlier

?boxplot
boxplot(adult$"capital.gain", xlab="capital gain")
boxplot(adult$"capital.loss", xlab="capital loss")
boxplot(adult$"hours.per.week", xlab="hours per week")
attach(adultD)# to work with the data without having to access the dF
summary(ocupation)
summary(workclass)
subset(adultD, workclass="?"  )
adultD$workclass=="?"
?subset
subset(adult, adult$"native.country"==" ?")
summary(adult)
summary(adult$"native.country" )
?plot
adult$income==" <=50K."
levels(education)
mahal<-Moutlier(adult[,c("age", "fnlwgt", "capital.gain", "capital.loss", "hours.per.week" )], plot=TRUE)
Moutlier(adult[,c("age", "fnlwgt")] )
