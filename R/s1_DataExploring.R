##
## step 1 to explore the data and work with outliers or null fields
##
adultD <- read.table("data/adult.data", head=FALSE, sep=",")
#set the variable names.
columns <- c("age", "workclass","fnlwgt","education","education-num","martial-status","ocupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","income")
colnames(adultD) <- columns 

summary(adultD)

#capital gain has errors?
table(adultD['capital-gain']==99999) # 159 equal to 99999 none bigger.
# this could be  the biggest number in the census form.

attach(adultD)# to work with the data without having to access the dF
