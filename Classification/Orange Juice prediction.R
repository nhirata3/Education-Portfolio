library(ISLR)
library(tidyverse)
library(dplyr)
data(OJ)
oj<-OJ
oj$Purchase<-factor(ifelse(oj$Purchase=="MM",1,0))
oj$Store7<-factor(ifelse(oj$Store7=="Yes",1,0))
summary(oj)
str(oj)
dim(oj)# 1070  18
##(a) Split to training (800 rows) and testing subset 
set.seed(456)
n<-dim(oj)[1]
n1<-n-800
flag <- sort(sample(1:n,n1, replace = FALSE))
ojtrain <- oj[-flag,]
ojtest <- oj[flag,]
dim(ojtrain)
dim(ojtest)
dim(ojtest[ojtest$Purchase ==1,])

ytrain    <- ojtrain$Purchase;
ytest    <- ojtest$Purchase

###EDA###
oj %>%
  keep(is.numeric)%>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=sqrt(nrow(oj))) 

###Correlation Matrix###
library(PerformanceAnalytics)
oj_corr<-oj
oj_corr$Purchase<-as.numeric(oj_corr$Purchase)
oj_corr$Store7<-as.numeric(oj_corr$Store7)
str(oj_corr)
chart.Correlation(oj_corr)

## (b) Fit Classification Tree with "Gini" criterion
library(rpart)
## grow tree
rpart.oj1 <- rpart(Purchase ~ .,data=ojtrain, method="class", parms=list(split="gini"))
summary(rpart.oj1)

## Initial Tree T0, we have 9 terminal nodes
print(rpart.oj1)
post(rpart.oj1,filename="")

## Training Errors for Tree T0
y1hatc <- ifelse(predict(rpart.oj1,ojtrain)[,2] < 0.5, 0, 1)
sum(y1hatc != ytrain)/length(ytrain)
#  0.155 (training error for T0)



## (c) Or simplified plot
plot(rpart.oj1,compress=TRUE)
text(rpart.oj1)



## (d) Predict response on Test data and produce confusion matrix
library(caret)
y2hatc <-  predict(rpart.oj1, ojtest[,-1],type="class")
sum(y2hatc != ytest)/length(ytest)
confusionMatrix(as.factor(y2hatc),ojtest$Purchase,positive="1")
# 0.1703704 (test error for T0)


##(e) Use the training set to determine the optimal tree size that corresponds to the lowest cross-
## validation classification error rate.

## To determine whether the tree T0 is appropriate or if some of 
##    the branches need to be pruned 
plotcp(rpart.oj1)

printcp(rpart.oj1)
## or 
print(rpart.oj1$cptable)

## The xerror column is the estimates of cross-validated prediction
##   error for different numbers of splits. 
##   Here the best tree turns out to be T0

opt <- which.min(rpart.oj1$cptable[, "xerror"]); 
cp1 <- rpart.oj1$cptable[opt, "CP"];
rpart.pruned1 <- prune(rpart.oj1,cp=cp1);
rpart.pruned1
y3hatc <-  predict(rpart.pruned1, ojtest[,-1],type="class")
sum(y3hatc != ytest)/length(ytest)
##0.1666667
confusionMatrix(as.factor(y3hatc),ojtest$Purchase,positive="1")
## This is same as T0 in this example. Maybe different in other problems

## Try another cp
cp1 <- 0.010;
rpart.pruned1 <- prune(rpart.oj1,cp=cp1);
y2hatc1 <-  predict(rpart.pruned1, ojtest[,-1],type="class")
sum(y2hatc1 != ytest)/length(ytest)
## 0.1703704 
#This makes sense since the default cp is 0.010.

## Compare T01 and T1
par(mfrow=c(1,2))
post(rpart.oj1,filename="")
post(rpart.pruned1,filename="")


## Alternatively, we can plot Complexity Parameter table 
##  A horizontal line is drawn 1 SE above the minimum of curve
##  A good choice of Cp for pruning is often the leftmost value
##  for which the mean lies below the horizontal line
plotcp(rpart.oj1)
cp2 <- 0.014063; 
rpart.pruned2 <- prune(rpart.oj1,cp=cp2);
y2hatc2 <-  predict(rpart.pruned2, ojtest[,-1],type="class")
sum(y2hatc2 != ytest)/length(ytest)
confusionMatrix(y2hatc2,ojtest$Purchase,positive = "1")
##0.155556

## Try another tree T2
rpart.pruned3 <- prune(rpart.oj1,cp=0.021875)
y2hatc3 <-  predict(rpart.pruned3, ojtest[,-1],type="class")
sum(y2hatc3 != ytest)/length(ytest)
confusionMatrix(y2hatc3,ojtest$Purchase,positive = "1")
## 0.2 (test error for T2) 



## Logistic regression
modlogistic <- glm( Purchase ~ ., family = binomial(link = "logit"), data= ojtrain);
phatlogistic  <-  predict(modlogistic, ojtest[,-1],type="response")
yhatlogistic <- ifelse(phatlogistic  <0.5,0,1)
sum(yhatlogistic  != ytest)/length(ytest)
confusionMatrix(as.factor(yhatlogistic),ojtest$Purchase,positive = "1")
##0.1444444


