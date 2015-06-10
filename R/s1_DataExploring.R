###################################################################
###                                                               #
###         MultiVariate     Analysis      Project                #
###                       Adult Project                           #
###       Héctor Apolo Rosales Pulido & Rocco Proscia             #
###                                                               #
###################################################################

#########################################################################################################
## step 1 to explore the data and work with outliers or null fields
#########################################################################################################
adultD <- read.table("data/adult.data", head=FALSE, sep=",")
adultDT <- read.table("data/adult.test", head=FALSE, sep=",")
levels(adultDT$V15) <- c(" <=50K"," >50K")
adult<-rbind(adultD, adultDT)# 48796 size
#set the variable names.
columns <- c("age", "workclass","fnlwgt","education","education.num","martial.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country","income")
colnames(adult) <- columns 
summary(adult)

########################################### WORK CLASS #################################################
#summary(adult$workclass)
#levels(adult$workclass)
# we will add a new category for workclass
levels(adult$workclass) <- c("noWClass","Federal-gov","Local-gov","Never-worked","Private","Self-emp-inc","Self-emp-not-inc","State-gov","Without-pay")
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
#summary(adult$native.country)
#########################################################################################################
########################################  Purge variables  ##############################################
summary ( subset(adult, ( adult$occupation=="noOccupation"&adult$"native.country"=="noCountry"&adult$workclass=="noWClass" ) ) )
indexToDelete<-as.numeric( rownames( subset(adult, ( adult$occupation=="noOccupation" & adult$"native.country"=="noCountry"&
                                                       adult$workclass=="noWClass" ) ) ) )
#DELETED THE ROWS WITH 3 VARIABLES NULL
adult<-adult[-indexToDelete,] # we end u with 48796 n size of Adult.
#########################################################################################################


#***************************************** PROBABLY USELESS *********************************************
summary(adult)
#capital gain has errors?
table(adult['capital.gain']==99999) # 159 equal to 99999 none bigger.
# this could be  the biggest number in the census form.
boxplot(adult$"capital.gain")
nrow( subset(adult, adult$"capital.gain">99000 ))
summary(adult)
levels(adult$education)

#########################################################################################################
################################## Delete continuous variables ##########################################
adult.temp <- adult;
adult <- adult.temp
adult = adult[-12] # delete capital.loss
adult = adult[-11] # delete capital.gain
adult = adult[-5] # delete education.num
################################## Discretize continuous variables ##########################################

adult$age <- (cut(adult$age, breaks = c(0, 25, 45, 65, Inf), labels = c("young","adult","senior","Old")))
adult$hours.per.week <- cut(adult$hours.per.week, breaks = c(0, 25, 40, 60, Inf), labels = c("underEmployed","normal","overworked","slaveLabor"))
levels(adult$hours.per.week)
levels(adult$race)
levels(adult$martial.status)
#########################################################################################################
################################## OutlierDetection ##########################################
adult[,c("age","fnlwgt", "education.num","hours.per.week")]
library(chemometrics)
adult.cont <- adult[,c("age","fnlwgt", "education.num","hours.per.week")]
hist(adult$"capital.gain")
hist(adult$"capital.loss")
mahal<-Moutlier(adult.cont, plot=TRUE)
#########################################################################################################
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

#########################################################################################################
#############################################  MCA  #####################################################
library(FactoMineR)

par(mfrow=c(2,2))
res.mca <- MCA( bar,quali.sup = bar[,10])

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
###### CHECKPOINT 2. #########
bar <- readRDS(file="data/AdultPruned.Rda")
summary(bar)
bar<-bar[-3]
bar<-bar[-10]
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
