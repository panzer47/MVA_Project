###################################################################
###                                                               #
###         MultiVariate     Analysis      Project                #
###                       Adult Project                           #
###       Héctor Apolo Rosales Pulido & Rocco Proscia             #
###                                                               #
###################################################################
#########################################################################################################
## libraries
#########################################################################################################
library(nortest)
library(MASS)
require(npmc)
library(discretization)
#########################################################################################################
## step 1 to explore the data and work with outliers or null fields
#########################################################################################################
adultD <- read.table("data/adult.data", head=FALSE, sep=",")
adultDT <- read.table("data/adult.test", head=FALSE, sep=",")
levels(adultDT$V15) <- c(" <=50K"," >50K")
adulto<-rbind(adultD, adultDT)# 48796 size
#set the variable names.
columns <- c("age", "workclass","fnlwgt","education","education.num","martial.status","occupation","relationship","race"
             ,"sex","capital.gain","capital.loss","hours.per.week","native.country","income")
colnames(adulto) <- columns 
str(adulto)
summary(adult)

########################################### WORK CLASS #################################################
#summary(adult$workclass)
#levels(adult$workclass)
# we will add a new category for workclass
levels(adult$workclass) <- c("noWClass","Federal-gov","Local-gov","Never-worked","Private",
                             "Self-emp-inc","Self-emp-not-inc","State-gov","Without-pay")
#summary(adult$workclass)
#########################################################################################################
###########################################  Occupation #################################################
#summary(adult$occupation)
#levels(adult$occupation)
# we will add a new category for workclass
levels(adult$occupation) <- c("noOccupation","Adm-clerical","Armed-Forces","Craft-repair","Exec-managerial"
  , "Farming-fishing", "Handlers-cleaners","Machine-op-inspct","Other-service","Priv-house-serv"
  , "Prof-specialty" ,"Protective-serv","Sales","Tech-support","Transport-moving")
#summary(adult$occupation)
#########################################################################################################
###########################################   country   #################################################
#summary(adult$native.country)
#levels(adult$native.country)
# we will add a new category for workclass
levels(adult$native.country) <- c("noCountry","Cambodia","Canada","China"              
                ,"Colombia","Cuba","Dominican-Republic","Ecuador"
                ,"El-Salvador","England","France","Germany"
                ,"Greece","Guatemala","Haiti","Holand-Netherlands"
                ,"Honduras","Hong","Hungary","India"
                ,"Iran","Ireland","Italy","Jamaica"
                ,"Japan","Laos","Mexico","Nicaragua"                
                ,"Outlying-US(Guam-USVI-etc)","Peru","Philippines","Poland"                   
                ,"Portugal","Puerto-Rico","Scotland","South"                    
                ,"Taiwan","Thailand","Trinadad&Tobago","United-States"            
                ,"Vietnam","Yugoslavia")
levels(adult$native.country) <- c("noCountry","Asia","United-States","Asia"              
                                  ,"LatinAmerica","LatinAmerica","LatinAmerica","LatinAmerica"
                                  ,"LatinAmerica","Europe","Europe","Europe"
                                  ,"Europe","LatinAmerica","LatinAmerica","Europe"
                                  ,"LatinAmerica","Asia","Europe","Asia"
                                  ,"Asia","Europe","Europe","LatinAmerica"
                                  ,"Asia","Asia","Mexico","LatinAmerica"                
                                  ,"United-States","LatinAmerica","Asia","Europe"                   
                                  ,"Europe","United-States","Europe","Asia"                    
                                  ,"Asia","Asia","LatinAmerica","United-States"            
                                  ,"Asia","Europe")

chisq.test(adult$native.country,adult$income)

chisq.test(adult$fnlwgt,adult$income)
str(adult)
levels(adult$native.country)
table(adult$native.country,adult$income)
#summary(adult$native.country)
#########################################################################################################
########################################  Purge variables  ##############################################
summary (subset(adult, (adult$occupation=="noOccupation"&adult$"native.country"=="noCountry"&adult$workclass=="noWClass")))
indexToDelete<-as.numeric( rownames( subset(adult, ( adult$occupation=="noOccupation" & adult$"native.country"=="noCountry"&
                                                       adult$workclass=="noWClass" ) ) ) )
#DELETED THE ROWS WITH 3 VARIABLES NULL
adult<-adult[-indexToDelete,] # we end u with 48796 n size of Adult.

################################## Discretize continuous variables ##########################################

str(adult)
adult$age <- (cut(adult$age, breaks = c(15,25,35, 45,55, 65, Inf), 
                  labels = c("young","youngAdult","adult","oldAdult","senior","retired")))
table(adult$age)
adult$hours.per.week <- cut(adult$hours.per.week, breaks = c(0, 20,30,40,50, 60, Inf), 
                            labels = c("underEmployed","lowNormal","normal","highNormal","overworked","slaveLabor"))

adult[[ "capital.gain"]] <- ordered(cut(adult[[ "capital.gain"]],
                      c(-Inf,0,median(adult[[ "capital.gain"]][adult[[ "capital.gain"]]>0]),
                      Inf)), labels = c("None", "Low", "High"))
table(adult$capital.gain)

adult[[ "capital.loss"]] <- ordered(cut(adult[[ "capital.loss"]],
                      c(-Inf,0, median(adult[[ "capital.loss"]][adult[[ "capital.loss"]]>0]),
                      Inf)), labels = c("None", "Low", "High"))
table(adult$native.country,adult$income)

ad.test(adult$fnlwgt)


table(adult$capital.gain,adult$income)
table(adult$capital.loss,adult$income)

plot(adult$capital.gain)
plot(adult$capital.loss)
table(adult$hours.per.week)
levels(adult$race)
levels(adult$martial.status)
#########################################################################################################
#########################################################################################################
################################## OutlierDetection ##########################################
adult[,c("age","fnlwgt", "education.num","hours.per.week")]
library(chemometrics)
adult.cont <- adult[,c("age","fnlwgt", "education.num","hours.per.week")]
hist(adult$"capital.gain")
hist(adult$"capital.loss")
mahal<-Moutlier(adult.cont, plot=TRUE)
#########################################################################################################

############################## CHECKPOINT 1. ##############################
############################## clean data and categories ##############################
saveRDS(adult, file="data/AdultCleaned.Rda")
adult <- readRDS(file="data/AdultCleaned.Rda")

#***************************************** PROBABLY USELESS *********************************************

wilcox.test(y~A) 
names(adult)
table(adult$capital.gain,adult$income)
aovEdu <- aov(education.num~education,data=adult)
aovEdu
lm1 <- lm(adult$education.num~adult$income)
ad.test(adult$education.num)
qqplot(adult$education.num,residuals(lm1))
table(adult$education,adult$education.num)


glm(adult$capital.gain~adult$income)
summary(lm(adult$capital.loss~adult$income))
xad.test(aovEdu)

glm(income~.,data=adult[,-c(12,11,5)])

hist(log(adult$education.num))
kruskal.test(education.num~education,data=adult)
kruskal.test(education.num~native.country,data=adult)



t.test(capital.gain~income,data=adult)
wilcox.test(capital.gain~income,data=adult) 
?wilcox.test
?kruskal.test
kruskal.test(education.num~adult$martia,data=adult)
?


summary(adult)
#capital gain has errors?
table(adult['capital.gain']==99999) # 159 equal to 99999 none bigger.
# this could be  the biggest number in the census form.
boxplot(adult$"capital.gain")
nrow( subset(adult, adult$"capital.gain">99000 ))
summary(adult)
levels(adult$education)

#########################################################################################################
####################################### Feature Selection ###############################################
chisq<-chiSq(adult)


######################################## deleting native.country ########################################


plot(adult$native.country)
nCountTable<-table(adult$native.country)
cbind(nCountTable,prop.table(nCountTable)*100) # we see that United States have almost 90% of the sample.
# also this is good for us because we have 43 levels in this factor, which will become a computational
# challenge later on.
adult = adult[-14] # delete native.country
#########################################################################################################
kruskal.test(capital.loss~income,data=adult)
adult = adult[-12] # delete capital.loss
library(lmtest)
dwtest(adult$capital.loss~adult$income)
dwtest(adult$capital.loss~rownames(adu))
adult = adult[-11] # delete capital.gain



######################################## deleting native.country ########################################
table(adult$education,adult$education.num)
adult = adult[-5] # delete education.num because is redundant, and will increase our bias
adult = adult[-3]

chisq.test(adult$education,adult$income,simulate.p.value = TRUE)
?chisq.test
?Moutlier

?boxplot
#######################################Analysis#########################################################
boxplot(adult$"capital.gain", xlab="capital gain")
boxplot(adult$"capital.loss", xlab="capital loss")
boxplot(adult$"hours.per.week", xlab="hours per week")
attach(adultD)# to work with the data without having to access the dF
detach(adult)
summary(ocupation)
summary(workclass)
subset(adultD, workclass="?"  )
adultD$workclass=="?"
levels(ocupation)
?subset
subset(adult, adult$"native.country"==" ?")
summary(adult)
summary(adult$"native.country" )
?plot
adult$income==" <=50K."
levels(education)
mahal<-Moutlier(adult[,c("age", "fnlwgt", "capital.gain", "capital.loss", "hours.per.week" )], plot=TRUE)
Moutlier(adult[,c("age", "fnlwgt")] )


t.test(income~native.country,data=adult)
#########################################################################################################
#######################################  Classification  ################################################
library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)
library(pROC)
library(caret)

# i am using the original training data, since we erased some rows that have too many unknown values, 
# the initial point of the Test data was changed, i detected where was the first one using adult.rowname(),
# to show where the initial opint was.
adult.tr <- adult[1:32534,]
adult.tst <- adult[32535:nrow(adult),]
part = rpart(income~ ., data=adult.tr, parms=list(split='gini'),  
             control=rpart.control(cp=0.001, xval=10, maxdepth=15))

print(part)
printcp(part)
plotcp(part)
summary(part)

par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(part) # visualize cross-validation results    
par(mfrow=c(1,1))

plot(part, uniform=TRUE, 
     main="Regression Tree for ADULT ")
text(part, use.n=TRUE, all=TRUE, cex=.8)

plot(part$cptable[,2],part$cptable[,3],type="l")
lines(part$cptable[,2],part$cptable[,4],col="blue")
legend("topright",c("R(T)training","R(T)cv"),col=c("black","blue"),lty=1)

alfa <- part$cptable[which.min(part$cptable[,4]),1]
alfa

p1 <- prune(part,cp=alfa)
p1
print(p1)
fancyRpartPlot(p1)
print(p1)
printcp(p1)
plotcp(p1)
plot(p1, uniform=TRUE,
     main="Regression Tree for ADULT ")
text(p1, use.n=TRUE, all=TRUE, cex=.8)


probsTrain <- predict(p1,adult.tr, type = "class")

incomPerc <- table(adult.tr$income)
(ct<-table(probsTrain,adult.tr$income))

perCT <- matrix(nrow=2,ncol=2)
row.names(perCT)<- row.names(ct)
colnames(perCT)<- colnames(ct)
for(i in 1:2){
  perCT[i,1] <- ct[i,1]/incomPerc[1]
  perCT[i,2] <- ct[i,2]/incomPerc[2]
}
perCT

(err <- 1.0 - (ct[1,1] + ct[2,2])/sum(ct))
View(probsTrain)
levels(adult.tr$income) <- c("<=50K",">50K")


###### THIS DOEES NOT WORK IDK WHY

roc <- roc(response = adult.tr$income,
           predictor = probsTrain[,"1"],
           levels = rev(levels(adult.tr$income)))
plot(roc, print.thres = "best")

View(probsTrain)# crashes R




####### XXXX VALIDATION ########
N <- nrow(adult.tr)
K <- 10
partition <- N%/%K
set.seed(1234)
rand <- runif(N)
range <- rank(rand)
block <- (range-1)%/% partition + 1
block <-as.factor(block)
summary(block)

sum.error <- numeric(0)
for(k in 1:K){
  tree <- rpart(income ~ ., data=adult.tr[block!=k,], parms=list(split='gini'),  
                control=rpart.control(cp=0.001, xval=10, maxdepth=15))
  pred <- predict(tree,newdata=adult.tr[block==k,], type = "class")
  ct<-table(adult.tr$income[block==k],pred)
  err <- 1.0 - (ct[1,1] + ct[2,2])/sum(ct)
  sum.error <- rbind(sum.error,err)
}

sum.error

(mean.error <- mean(sum.error)) # 0.1659084   mean error


#########################################################################################################
#############################################  MCA  #####################################################
library(FactoMineR)

par(mfrow=c(2,2))
res.mca <- MCA( bar,quali.sup = bar[,13])

str(bar)
bar<-bar[,-3]
summary(bar)
str(adult)

mean<-mean(res$eig$eigenvalue)
res$eig>mean
#dimdesc(res, axes=1:26, proba=0.05)
dimdesc(res)
##### Save data for later use instead of running all the code
saveRDS(adult, file="data/AdultPruned.Rda")
############################## CHECKPOINT 2. ##############################
adult <- readRDS(file="data/AdultPruned.Rda")
#########################################################################################################

#age and income are not independent
chisq.test(adult$education,adult$income) 
#age and income are not independent
chisq.test(adult$sex,adult$income)
#age and income are not independent
chisq.test(adult$adult,adult$income)
#age and income are not independent
chisq.test(adult$occupation,adult$relationship)

require(dplyr)
library(kernlab)
adultM1 <- glm (income ~ ., data=adult, family=binomial)
adultM1.AIC <- step (adultM1)

mutate(group_by(adult$native.country,size),am_pcnt = amount/sum(amount))
?plot
plot(adult$native.country)
plot(adult$race)
(mytable<-table(adult$native.country))

summary(adult$age)

for(i in 1:12){
  print(chisq.test(adult[i],adult$income,simulate.p.value = TRUE)$p.value)
}

library(e1071)
model <- svm(income~., data=adult)
?svm

###### CHECKPOINT 2. #########
bar <- readRDS(file="data/AdultPruned.Rda")
summary(bar)
###CHISQUARE, IS GOOD???!?!?!?
chisq.test(bar$age, bar$race)



x<-sample(nrow(bar),5000)
mat<-as.matrix(bar[1:5000,])
library(cluster)
d<-dist(mat, method="gower")
d<-daisy(bar[1:5000,], metric="gower")

##LETS DO SOME CLUSTERING
clustering<-hclust(d, method="ward.D2")
plot(clustering)
barplot(clustering$height[4940:5000])
?plot
library(rattle)
centroids<-centers.hclust(d , clustering, nclust=2, use.median=FALSE)
?kmeans
kmeans<-kmeans(d, centroids)
summary(kmeans)
catdes(cbind( bar[1:5000,], as.factor(kmeans$cluster)) , num.var=10 )
