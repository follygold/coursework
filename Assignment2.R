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

#################PROBLEM FOUR##############
# correlation between the change and Calm variables

cor_change_calm <- cor(looscan$change, looscan$Calm)

cor_change_calm

#############PROBLEM FIVE##################
# scatterplot of change and calm variable

library(ggplot2)#load the respective package
sp <- ggplot(looscan, mapping = aes(x = Calm, y = change, color = Age)) + 
  geom_point(size = 2,shape = 23) + geom_smooth(method ='loess', se = FALSE)

sp 

##############PROBLEM SIX########################3
#train the Ripper rule-based model using the entire dataset

install.packages("RWeka")
library(RWeka)

ripper_model <- JRip(Gender~., data = looscan)

###############PROBLEM SEVEN##################
ripper_model

#####################PROBLEM EIGHT##################
summary(ripper_model) #IT DISPLAYS CONFUSION MATRIX TOO

##############PROBLEM NINE################
# Discretize the Change variable into a factor of 3 values

looscan$change <- ifelse(looscan$change < 0, "Decrease", ifelse(looscan$change == 0, "noChange", "Increase"))

looscan$change <- as.factor(looscan$change) # convert the discretized change variable to a factor

################PROBLEM TEN#################
# 10. Ripper rule-based model  with change as the target variable

ripper_model2 <- JRip(change~., data = looscan)

##############PROBLEM ELEVEN###################
ripper_model2#######PRINT THE MODEL

summary(ripper_model2)############DISPLAY THE SUMMARY



 
