######load data into r
pep<-read.csv("peptide(1).csv")
pep
#remove unwanted columns
library(dplyr)
df = subset(pep, select = -c(Fraction,Source.File,X.Spec.Sample.41,PTM,AScore))
df            
dim(df)
#load necessary package
install.packages("tidyr")
library(tidyr)
#separate ko range into start values and end values
peps=separate(data= df, col=X1.K0.Range, into =c("Start","End"),sep="-",remove=TRUE,convert=TRUE)
peps
dim(peps)
#find difference 
peps$diff_range=(peps$End)-(peps$Start)
peps
dim(peps)
#reorder the columns
my_data2 <- peps[, c(1,2,3,4,5,6,7,8,9,14,10,11,12,13)]
my_data2
########calculate five number summary of thwe mass variable
massum<-summary(my_data2$Mass)
massum
#create the boxplot of variable length
boxplot(my_data2$Length)
#create a histogram for variable m/z
hist(my_data2$m.z)
#Create a scatterplot to show the bivariate relation between “m/z” and “RT”,
#using “m/z” as the independent variable and “RT” the dependent variable
library(car)
scatterplot(my_data2$m.z,my_data2$RT,data=my_data2)
#8.	Tabularize the values in variable “#Spec” using function table().
spectable<-table(my_data2$X.Spec)
spectable
#save code to word document
sink("datout.doc",append=TRUE)
sink()
#10.	Normalize all numeric columns 
#do not notmalize non numeric columns
#list out the classes of columns
lapply(my_data2,class)
#remove non-numric colums
my_data3 = subset(my_data2, select = -c(Peptide,Length,Intensity.Sample.41,Precursor.Id,X.Spec,Accession))
my_data3
#normalize the new data                  
install.packages("BBmisc")
library(BBmisc)
normalize(my_data3, method = "range", range = c(0, 1), margin = 2L)
write.csv(my_data3,'data3.csv')
