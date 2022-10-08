library(class)
library(rlist)
library(caret)
library(tidyverse)
library(dplyr)
library(PerformanceAnalytics)
bank_train<- read.table(file="C:/Users/nhirata/Desktop/ISYE7406/Project/bank-full.csv",sep=";",stringsAsFactors = TRUE,header=TRUE)
bank_test<-read.table(file="C:/Users/nhirata/Desktop/ISYE7406/Project/bank.csv",sep=";",stringsAsFactors = TRUE, header=TRUE)
n<-dim(bank_train)[1]
set.seed(777)
n1 = round(n/10); ### number of observations randomly selected for testing data
str(bank_train)
bank_train$y<-factor(ifelse(bank_train$y=="yes",1,0));bank_test$y<-factor(ifelse(bank_test$y=="yes",1,0))
ytrain<- bank_train$y;
ytest<- bank_test$y;
set.seed(777); ### set the seed for randomization
### Initialize the TE values for all models in all $B=100$ loops
B= 100; ### number of loops

###EDA###
#Based on Factors
summary(bank_train)
bank_train %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value, fill=value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  theme(legend.position="none")
#Based on Numeric
bank_train %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value, fill=value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  theme(legend.position="none")
###correlation###
bank_train1<-bank_train
bank_train1[, c(2:5,7:9,11,16,17)] <- sapply(bank_train1[, c(2:5,7:9,11,16,17)], as.numeric)
str(bank_train1)
my_data <- bank_train1[, c(17,12,8,16,4)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

###LOGISTIC REGRESSION###

## Both R code lead to the same results, the default link for binomial is "logit"
glm_model <- glm(y ~ ., data = bank_train, family = binomial(link = "logit"))
summary(glm_model)
str(glm_model)
# glm_model <- glm(y ~ default+contact+month+day_of_week+duration+pdays+poutcome+emp.var.rate+cons.price.idx+euribor3m+nr.employed, data = bank_train, family = binomial(link = "logit"))
# summary(glm_model)
probs <- predict(glm_model, newdata=bank_test, type = "response")
pred.glm <- rep(0, length(probs))
dist<-bank_train%>%
  count(y)
dist
# cstar<-1-(as.numeric(dist[[2,2]])/as.numeric(dist[[1,2]]))
cstar<-0.50
pred.glm[probs >= cstar] <- 1
mean(pred.glm != bank_test$y)
confusionMatrix(as.factor(pred.glm),bank_test$y,positive="1")
## 0.09776598 if we used all predictors.

###LOGISTIC REGRESSION CROSS VALIDATION (OPTION 1)###
set.seed(777)
te1<-NULL
for (b in 1:B){
  flag <- sort(sample(1:n, n1));
  bank_train_temp <- bank_train[flag,]; ## temp training set for CV
  bank_test_temp <- bank_train[-flag,]; ## temp testing set for CV
  
  ## Method 4: Logistic Regression
  ## Both R code lead to the same results, the default link for binomial is "logit"
  glm_model <- glm(y ~ ., data = bank_train_temp, family = binomial(link = "logit"))
  dist<-bank_train_temp%>%
    count(y)
  # cstar<-1-(as.numeric(dist[[2,2]])/as.numeric(dist[[1,2]]))
  cstar<-0.50
  probs <- predict(glm_model, bank_test_temp, type = "response")
  pred.glm <- rep(0, length(probs))
  pred.glm[probs >= cstar] <- 1
  te1<-c(te1,mean(pred.glm != bank_test_temp$y))
}
te1<-data.frame(te1)
apply(te1,2,mean)
apply(te1,2,var)

###LOGISTIC REGRESSION CROSS VALIDATION (OPTION 2)###
###TO RETRIEVE ACCURACY PERFORMANCE###
# define training control
train_control <- trainControl(method = "cv", number = 100)

# train the model on training set
model <- train(y ~ .,
               data = bank_train,
               trControl = train_control,
               method = "glm",
               family=binomial())

# print cv scores
model$results


###Random Forest: 0.000000 Perfect Classification###
set.seed(777)
library(randomForest)
modF <- randomForest(as.factor(y) ~., data=bank_train, 
                     importance=TRUE)
y2hatF = predict(modF, bank_test, type='class')
mean(y2hatF!=bank_test_temp$y)
mean(y2hatF != ytest)
confusionMatrix(as.factor(y2hatF),bank_test$y,positive="1")

te3<-NULL
B<-10
for (b in 1:B){
  n<-dim(bank_train)[1]
  n1<- round(n/10)
  flag <- sort(sample(1:n, n1));
  bank_train_temp <- bank_train[flag,]; ## temp training set for CV
  bank_test_temp <- bank_train[-flag,]; ## temp testing set for CV
  modF <- randomForest(as.factor(y) ~., data=bank_train_temp, 
                       importance=TRUE)
  y2hatF = predict(modF, bank_test_temp, type='class')
  te3 <- c(te3,  mean(y2hatF!=bank_test_temp$y))
  
}
te3<-data.frame(te3)
summary(te3)
apply(te3,2,mean)
apply(te3,2,var)

###RANDOM FOREST (USING FULL TRAINING DATA SET TO CREATE MODEL AND TEST DATA SET FOR PREDICTION)
te4<-NULL
B<-10
for (b in 1:B){
  bank_train_temp <- bank_train; ## temp training set for CV
  bank_test_temp <- bank_test; ## temp testing set for CV
  modF <- randomForest(as.factor(y) ~., data=bank_train_temp, 
                       importance=TRUE)
  y2hatF = predict(modF, bank_test_temp, type='class')
  te4 <- c(te4,  mean(y2hatF!=bank_test_temp$y))
  
}
te4<-data.frame(te4)
summary(te4)
te4
apply(te4,2,mean)
apply(te4,2,var)




### BOOSTING ### 
set.seed(777)
library(gbm)

# 
bank_train$y<-as.numeric(ifelse(bank_train$y==1,1,0));bank_test$y<-as.numeric(ifelse(bank_test$y==1,1,0))
gbm.bank1 <- gbm(y ~ .,data=bank_train,
                 distribution = 'bernoulli',
                 n.trees = 8000, 
                 shrinkage = 0.01, 
                 interaction.depth = 3,
                 cv.folds = 10)

## Model Inspection 
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.bank1, method="cv") 
perf_gbm1

## summary model
## Which variances are important
summary(gbm.bank1)


## Make Prediction
## use "predict" to find the training or testing error

## Training error
pred1gbm <- predict(gbm.bank1,newdata = bank_train, n.trees=perf_gbm1, type="response")
pred1gbm[1:10]
y1hat <- ifelse(pred1gbm < 0.5, 0, 1)
y1hat[1:10]
sum(y1hat != ytrain)/length(ytrain)  ##Training error = 0.07679547

## Testing Error
y2hat <- ifelse(predict(gbm.bank1,newdata = bank_test[,-17], n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
mean(y2hat != ytest) 
## Testing error = 0.07697412
bank_train$y<-as.factor(ifelse(bank_train$y==1,1,0));bank_test$y<-as.factor(ifelse(bank_test$y==1,1,0))
confusionMatrix(as.factor(y2hat),bank_test$y,positive="1")



## Method 3: Naive Bayes
##  This has been implemented in the R library "e1071"
##  You need to first install this library 
##
library(e1071)
TrainErr <- NULL;
TestErr  <- NULL; 
mod3 <- naiveBayes( bank_train[,1:16], bank_train[,17])
## Training Error
pred3 <- predict(mod3, bank_train[,1:16]);
TrainErr <- c(TrainErr, mean( pred3 != bank_train$y))
TrainErr 
##  0.1229568 for miss.class.train.error of Naive Bayes
## Testing Error 
TestErr <- c(TestErr, mean( predict(mod3,bank_test[,1:16]) != bank_test$y))
TestErr
##  0.1282051 for miss.class.test.error of Naive Bayes 
yhat<-predict(mod3,bank_test,type='class')
table(predict(mod3,bank_test[,1:16]),bank_test$y)
confusionMatrix(as.factor(yhat),bank_test$y,positive="1")

### CROSS VALIDATION USING TRAINING DATA AS FULL-DATASET (since there are 45K rows)###
te2<-NULL
for (b in 1:B){
  flag <- sort(sample(1:n, n1));
  n<-dim(bank_test)[1]
  n1<- round(n/10)
  bank_train_temp <- bank_test[flag,]; ## temp training set for CV
  bank_test_temp <- bank_test[-flag,]; ## temp testing set for CV

  ## Method 3: Naive Bayes
  mod2 <- naiveBayes( bank_train_temp[,1:16], bank_train_temp[,17])
  te2 <- c(te2,  mean( predict(mod2,bank_test_temp[,1:16]) != bank_test_temp$y))

  }
te2<-data.frame(te2)
apply(te2,2,mean)
apply(te2,2,var)
















