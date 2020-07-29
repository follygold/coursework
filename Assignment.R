##################PROBLEM ONE################

###########read the features data and the objects data into our R environment

features <- read.csv("features(1).csv")

object <- read.csv("objects.csv",header=TRUE)


##################PROBLEM TWO################


summary(features) # there appears to be NAs and invalid values in our data

summary(object)  # the minimum value appears to be zero but we do not need that category in our target variable


# get rid of the rows with NAs and invalid values in the features dataframe

View(features) # after carefully observing the data row 250 seem to be the only row with bad data

features <- features[-250,]

str(features) # the class of delta variable appears to be misrepresented

features$delta <- as.numeric(features$delta)

str(features)


# get rid of the zero value from the object data

object <- object$Object[object$Object != 0] 

object <- data.frame(object) # converting thr data back to a dataframe

object$object <- as.factor(object$object) # change the class of the object variable in the dataframe to a factor with five levels

str(object)




###########QUESTION 3#REMOVING COLINEARITY IN THE FEATURE DATASET 

library(caret)

highlyCor <- findCorrelation(cor(features),cutoff=0.8,verbose=TRUE)

highlyCor

# remove the highly correlated variables

features <- features [, - highlyCor] 

features

dim(features)


###################problem 4########### 
set.seed(1) # set seed for reproducibility

featuresampling <- sample(2,nrow(features),replace=T,prob=c(0.9,0.1))

featuresampling

training <- features[featuresampling == 1,]

training

testing <- features[featuresampling == 2,]

testing

##SPLITTING OBJECT DATASET THE SAME WAY

set.seed(1)  # set seed for reproducibility

objectsampling <- sample(2,nrow(object),replace=T,prob=c(0.9,0.1))

objectsampling

trainobj <- object[objectsampling == 1,]

trainobj 

testobj <- object[objectsampling == 2,]

testobj



############problem 5

# install.packages("C50")

library(C50)

tree_mod <- C5.0(y = trainobj, x = training, trials = 10)

tree_mod


####problem 6

#save the summary of the decision model
dec_sum<-summary(tree_mod)
dec_sum

###save the confusion matrix

pred<-predict(tree_mod,training)
pred
confmatrobj<-confusionMatrix(pred,trainobj)
confmatrobj

####problem 7
#use model to predict target features of testing dataset
predictions<-predict(tree_mod,testing)
predictions

########problem 8
library(gmodels)
CrossTable(predictions,testobj,prop.chisq = FALSE)
