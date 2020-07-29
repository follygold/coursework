#load the features and slee dataset
featureslee<-read.csv("features-Sleep.csv")
stageslee<-read.csv("stages-Sleep.csv")

#remove invalid data
str(featureslee)
str(stageslee)


#check for na values
summary(stageslee)
stageslee$stage<-as.factor(stageslee$stage)#convert to factor
stageslee[!complete.cases(stageslee),]
###########QUESTION 3#REMOVING COLINEARITY IN THE FEATURE DATASET 

library(caret)

highlyCorfeatlee <- findCorrelation(cor(featureslee),cutoff=0.8,verbose=TRUE)

highlyCorfeatlee


#normalize every column
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}
featureslee<- as.data.frame(lapply(featureslee, normalize))

######add stageslee to features dataset
features<-cbind(featureslee,stageslee)
features


#######randomly samle into training and testing dataset
set.seed(1) # set seed for reproducibility

featuresampling <- sample(2,nrow(features),replace=T,prob=c(0.8,0.2))

featuresampling

training <- features[featuresampling == 1,]

training

testing <- features[featuresampling == 2,]

testing

#train a model using gaussiam rbf model
install.packages("kernlab")
library(kernlab)
featclass<-ksvm(stage~.,data=training,kernel="rbfdot")
featclass
#train the model using testing dataset
feattest<-ksvm(stage~.,data=testing,kernel="rbfdot")
feattest
#create a cross table
featpredict<-predict(featclass,testing)
featpredict
library(gmodels)
featcross<-CrossTable(testing$stage,featpredict)
featcross






####################repeat steps 1-7 for the hands-grasp dataset####################
#load the hand gras dataset
feathandgra<-read.csv("features-HandGrasp.csv")
objhandsgra<-read.csv("objects-HandGrasp.csv")


str(feathandgra)
#convert to numeric
feathandgra$C1_delta_amp <- as.numeric(feathandgra$C1_delta_amp)
feathandgra$FC1_delta_amp <- as.numeric(feathandgra$FC1_delta_amp)
feathandgra$P1_delta_amp <- as.numeric(feathandgra$P1_delta_amp)
feathandgra$P3_delta_amp <- as.numeric(feathandgra$P3_delta_amp) 
str(feathandgra)
#remove invalid data
str(objhandsgra)
summary(feathandgra)#checking if there atre na values
feathandgra[!complete.cases(feathandgra),]#determine how many na values in the dataset
#row 250 has na values, it needs to be removed
feathandgra <- feathandgra[-250,]

#check for obj and remove na values
summary(objhandsgra)
objhandsgra <- objhandsgra[-250,]
summary(objhandsgra)
str(objhandsgra)
#convert obj to factor
objhandsgra<-as.factor(objhandsgra)
####remove collinearity
highlyCorfeathand <- findCorrelation(cor(feathandgra),cutoff=0.8,verbose=TRUE)

highlyCorfeathand

#normalize every column of the dataset
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}
feathandgra<- as.data.frame(lapply(feathandgra, normalize))
feathandgra
####add obj
featuresgra<-cbind(feathandgra,objhandsgra)
featuresgra

#randomly samling 8o%of the dataset ino training and testing
set.seed(1) # set seed for reproducibility

featuresamplingra <- sample(2,nrow(featuresgra),replace=T,prob=c(0.8,0.2))

featuresamplingra

trainingra <- featuresgra[featuresamplingra == 1,]

trainingra

testingra <- featuresgra[featuresamplingra == 2,]

testingra


#train a svm model using 
library(kernlab)
featclassgra<-ksvm(objhandsgra~.,data=trainingra,kernel="rbfdot")
featclassgra
#test the model
feattestgra<-ksvm(objhandsgra~.,data=testingra,kernel="rbfdot")
feattestgra
#create a cross tasble
featpredictgra<-predict(featclassgra,testingra)
featpredictgra
library(gmodels)
featcrossgra<-CrossTable(testingra$objhandsgra,featpredictgra)
featcrossgra
