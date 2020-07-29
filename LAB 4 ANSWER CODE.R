## 1.Loading data 
bgfeat<-read.csv("BG_features.csv")
bgfeat
bgobj<-read.csv("BG_objects.csv")
bgobj
fzfeat<-read.csv("FZ_features.csv")
fzfeat
fzobj<-read.csv("FZ_objects.csv")
fzobj
mjfeat<-read.csv("MJ_features.csv")
mjfeat
mjobj<-read.csv("MJ_objects.csv")
mjobj
mrobj<- read.csv("MR_objects.csv")
mrobj
yhfeat<-read.csv("YH_features.csv")
yhfeat
yhobj<-read.csv("YH_objects.csv")
yhobj


## 2.	Randomly sample 80% of the BG_features dataset and save them in a training dataset, and save the remaining 20% in a testing dataset. Split the BG_objects dataset in the same way.
set.seed(1456)
trainsample<- sample(nrow(bgfeat),nrow(bgfeat)*0.8)
trainsample
bgfeattrain<- bgfeat[trainsample,]
bgfeattrain
bgfeattest<- bgfeat[-trainsample,]
bgfeattest

#BGO
set.seed(1456)
trainsampleobj <- sample(nrow(bgobj),nrow(bgobj)*0.8)
bgobjtrain<- bgobj[trainsampleobj,]
bgobjtrain
bgobjtest <- bgobj[-trainsampleobj,]
bgobjtest

#3. Repeat Steps 1-3 for the other 4 datasets, viz. 
#FZF
set.seed(657)
trainsamplefzf <- sample(nrow(fzfeat),nrow(fzfeat)*.8)
fzfeattrain <- fzfeat[trainsamplefzf,]
fzfeattrain
fzfeattest <- fzfeat[-trainsamplefzf,]

#FZ

set.seed(1456)
fztrainsampleobj <- sample(nrow(fzobj),nrow(fzobj)*.8)
fztrainsampleobj
train_FZO <-fzobj[fztrainsampleobj,]
test_FZO <- fzobj[-fztrainsampleobj,]


#MJF and objects

#MJF
set.seed(1456)
train_samplemjf<- sample(nrow(mjfeat),nrow(mjfeat)*.8)
train_MJF <- mjfeat[train_samplemjf,]
test_MJF <- mjfeat[-train_samplemjf,]

#MJO

set.seed(1456)
train_sample_objectsmj <- sample(nrow(mjobj),nrow(mjobj)*.8)
train_MJO<- mjobj[train_sample_objectsmj,]
test_MJO <- mjobj[-train_sample_objectsmj,]


#MR_features and objects

#MRF
set.seed(1456)
train_samplemr <- sample(nrow(mrfeat),nrow(mrfeat)*.8)
train_MRF <- mrfeat[train_samplemr,]
test_MRF <- mrfeat[-train_samplemr,]

#MRO
set.seed(1456)
train_sample_objectsmro <- sample(nrow(mrobj),nrow(mrobj)*.8)
train_MRO <- mrobj[train_sample_objectsmro,]
test_MRO <- mrobj[-train_sample_objectsmro,]


#YH_features and objects

#YHF

set.seed(1456)
train_sampleyh <- sample(nrow(yhfeat),nrow(yhfeat)*.8)
train_YHF <- yhfeat[train_sampleyh,]
test_YHF <-yhfeat[-train_sampleyh,]

#YHO
set.seed(1456)
train_sample_objectsyh <- sample(nrow(yhobj),nrow(yhobj)*.8)
str(train_sample_objects)
train_YHO <- yhobj[train_sample_objectsyh,]
test_YHO <- yhobj[-train_sample_objectsyh,]

#4. Combine the 5 training datasets into one training dataset row-wisely 
#Features 
training_features <- rbind(bgfeattrain,fzfeattrain, train_MJF, train_MRF, train_YHF)


testing_features <- rbind(bgfeattest,fzfeattest, test_MJF, test_MRF, test_YHF)

#Objects 
training_objects <- c(bgobjtrain, train_FZO, train_MJO, train_MRO, train_YHO)

testing_objects <- c(bgobjtest, test_FZO, test_MJO, test_MRO, test_YHO)



#Labels
training_label <- as.factor(training_objects)
testing_label <- as.factor(testing_objects)

#Step 5.	Train the NaÃ¯ve Bayes model using the training dataset. Set laplace = 1.
library(e1071)
training <-naiveBayes (training_features, training_label, laplace=1)

#Step 6.	Use the Naïve Bayes model to predict the target feature of the testing dataset.
testing_predict <- predict(training,testing_features, type="class")
testing_predict


#Step 7.	Use CrossTable() function to compare the predicted object numbers to the true object numbers and analyze the results
library (gmodels)

CrossTable(x = testing_label, y = testing_predict,prop.chisq=FALSE,prop.t=FALSE,dnn=c('predicted', 'actual'))
