#After loading the file in the other document we continue with this file.
unkValues <- adult[workclass==" ?",]
names(adult)

for(1:dim(unkValues))
  
income[unkValues,]

slaves <-adult['hours-per-week'] >90
slaves
adult$"hours-per-week" ==90

#> names(adult)
#[1] "age"            "workclass"      "fnlwgt"         "education"      "education.num"  "martial-status"
#[7] "occupation"     "relationship"   "race"           "sex"            "capital-gain"   "capital-loss"  
#[13] "hours-per-week" "native-country" "income"        
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

adult.p1 <- adult[1:8]
adult.p2 <- adult[9:length(adult)]
var = 0
for( i in 1:length(occupation)
)
{
  #print(i)
  # very very very slow haha couldn't find another way :(
  if(occupation[i] ==" ?")  
    var[i] = TRUE
  else
    var[i] = FALSE
}

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


table(adult$occupation[i] ==" ?")
var <- as.factor(var)
var <- as.data.frame(var)
colnames(var) <- c("noOccupation")
adult <- cbind(adult.p1,var,adult.p2)
summary(adult$noOccupation)
names(adult)
