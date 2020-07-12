

#####
library(data.table)
library(party)
library(rpart)
library(rpart.plot)
library(randomForest)
library(stringr)
library(dplyr)
library(tidyr)
#####

churn=read.csv('churn.csv', head=T)

###

churn$Churn=str_replace(churn$Churn, 'loyal', 'No')
churn$Churn=str_replace(churn$Churn, 'churn', 'Yes')

##there are no 'NA'-s
(na_=churn[rowSums(is.na(churn))>0,])

summary(churn)
glimpse(churn)

churn$Churn=as.factor(churn$Churn)



##splitting data on 80% train and 20% test data
set.seed(1)
sample=sample.int(n=nrow(churn), size = floor(0.8*nrow(churn)), replace=F)
train=churn[sample,]
test=churn[-sample,]
View(train)
View(test)
dim(test)
dim(train)

###Decision Tree

##Creating decision tree model
names(train)
tree=ctree(train$Churn~Gender+Age+Payment.Method+LastTransaction, train)
plot(tree)
plot(tree, type='simple')
predictions=predict(tree, test)

## Confusion table and error rate
table(predictions, test$Churn)
mean(predictions==test$Churn)
(100+51)/180 ##Also

#so model predicts 83.9% correctly in other words it 16.1% is the training error rate


###Random Forest

##creating random forest model
RF=randomForest(Churn~.,train)
RF
prediction_RF=predict(RF, test)
## Confusion table and error rate

table(prediction_RF,test$Churn)
mean(prediction_RF==test$Churn)
(95+50)/180

#In this case, random forest predicts 80.6% correctly, this means train data error is 19.4%
errorDIFF=((100+51)/180-(95+50)/180)*100
errorDIFF
##So, the first model is better than the second one, since it is 3.3% more accurate 
