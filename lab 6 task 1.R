###################PROBLEM ONE#############
###install the package
#load the data

install.packages("readxl")
library(readxl)  

looscan <- read_excel("Looscan.xlsx")

str(looscan)#checking for structure

# converting the Gender and Age variables to factors

looscan$Gender <- as.factor(looscan$Gender)

looscan$Age <- as.factor(looscan$Age)

str(looscan)

###############PROBLEM TWO#######################
# creating pre-learning, post-learning and change variables

prelt <- looscan$`Pre-Time`/ looscan$`Pre-Count`

looscan$`Post-Count`[looscan$`Post-Count` == 0] <- 0.5  # changing zero values to 0.5 to avoid zero division

postlt <- looscan$`Post-Time`/looscan$`Post-Count`

change <- prelt - postlt

# adding the three variables to the dataset

looscan <- cbind(looscan, prelt, postlt, change)

looscan

#######PROBLEM THREE#########
# Erase Pre-Time, Pre-Cont, Post-Time, Post-Count from the dataset

looscan$`Pre-Time` <- NULL
looscan$`Post-Time` <- NULL
looscan$`Pre-Count` <- NULL

looscan


#####PROBLEM FOUR############
#CREATE AN ENHANCED SCATTERPLOT MATRIX USING PANELS FUNCTION IN THE PSYCH PACKAGE
install.packages("psych")
library(psych)
sp<-pairs.panels(looscan,pch=21,main="Scatterplot of looscan data")
sp

############PROBLEM FIVE################
#TRAIN THE LINEAR REGRESSION MODEL OF THE ENTIRE MODEL
linearmod<-lm(change~.,data=looscan)
linearmod


############PROBLEM SIX#################
#PRINT THE SUMMARY
summary(linearmod)


#############PROBLEM SEVEN###############
#ADD A QUADRATIC TERM OF CALM INTO THE DATASET
looscan$calm2<-looscan$Calm^2
looscan$calm2


############PROBLEM EIGHT################
#ADD A QUADRATIC TERM OF ACTIVE INTO THE DATASET
looscan$Active2<-looscan$Active^2
looscan$Active2

#######problem nine############
#add an interaction between calm and birds in the dataset
looscan$CB<-looscan$Calm*looscan$Birds
looscan$CB


############train the linear model of the entire set
looscannewmodel<-lm(change~.,data=looscan)
looscannewmodel


############PROBLEM ELEVEN#######
#PRINT THE MODEL SUMMARY
summary(looscannewmodel)

