Auto1 <- read.table(file = "C:/Users/nhirata/Desktop/ISYE7406/Homework3/Auto.csv", sep = ",", header=T);
mpg01 = I(Auto1$mpg >= median(Auto1$mpg))
Auto = data.frame(mpg01, Auto1[,-1]); ## replace column "mpg" by "mpg01".

## Explore the data graphically
summary(Auto)
#Scatter Plot
par(mfrow=c(3,3))
plot(Auto$mpg01~Auto$cylinders,pch=19,col="blue") 
plot(Auto$mpg01~Auto$displacement,pch=19,col="blue") #sig
plot(Auto$mpg01~Auto$horsepower,pch=19,col="blue") #sig
plot(Auto$mpg01~Auto$weight,pch=19,col="blue") #sig
plot(Auto$mpg01~Auto$acceleration,pch=19,col="blue") #sig
plot(Auto$mpg01~Auto$year,pch=19,col="blue")
plot(Auto$mpg01~Auto$origin,pch=19,col="blue")
par(mfrow=c(1,1))
plot(Auto$weight~Auto$horsepower,pch=19,col="blue")

##Correlation Plot
library("PerformanceAnalytics")
chart.Correlation(Auto, histogram=TRUE, pch=19)

#SPLIT DATA INTO TRAINING AND TEST SETS#
n = dim(Auto)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data; ~10% of my data
RNGkind(sample.kind = "Rounding")
set.seed(888); ### set the seed for randomization
flag = sort(sample(1:n, n1));
auto.train = Auto[-flag,] 
auto.test = Auto[flag,]
auto.train$mpg01 <- as.factor(auto.train$mpg01); 

#USE ONLY VARIABLES THAT SEEMED MOST ASSOCIATED WITH MPG01
auto.train = auto.train[c("mpg01","cylinders","horsepower","weight","displacement","origin")]
auto.test = auto.test[c("mpg01","cylinders", "horsepower","weight","displacement","origin")]

TrainErr <- NULL;
TestErr  <- NULL; 
### Method 1: LDA
library(MASS)
# fit1 <- lda( y ~ ., data= auto.train, CV= TRUE)
mod1 <- lda(auto.train[,2:6], auto.train[,1]); 

## training error 
## we provide a detailed code here 
pred1 <- predict(mod1,auto.train[,2:6])$class; 
TrainErr <- c(TrainErr, mean( pred1  != auto.train$mpg01)); 
TrainErr; 
## 0.09348442 for miss.class.train.error
## testing error 
pred1test <- predict(mod1,auto.test[,2:6])$class; 
TestErr <- c(TestErr,mean(pred1test != auto.test$mpg01));  
TestErr;
## 0.1538462 for miss.class.test.error

## You can also see the details of Testing Error
##     by the confusion table, which shows how the errors occur
table(pred1test,  auto.test$mpg01) 


## Method 2: QDA
mod2 <- qda(auto.train[,2:6], auto.train[,1])
## Training Error 
pred2 <- predict(mod2,auto.train[,2:6])$class
TrainErr <- c(TrainErr, mean( pred2!= auto.train$mpg01))
TrainErr
## 0.10481586 for miss.class.train.error of QDA,
##  which is much smaller than LDA
##  Testing Error 
pred2test<-predict(mod2,auto.test[,2:6])$class;
TestErr <- c(TestErr, mean(pred2test != auto.test$mpg01))
TestErr
## 0.1282051  for miss.class.test.error
table(pred2test,auto.test$mpg01)

## Method 3: Naive Bayes
##  This has been implemented in the R library "e1071"
##  You need to first install this library 
##
library(e1071)
mod3 <- naiveBayes( auto.train[,2:6], auto.train[,1])
## Training Error
pred3 <- predict(mod3, auto.train[,2:6]);
TrainErr <- c(TrainErr, mean( pred3 != auto.train$mpg01))
TrainErr 
##  0.09631728 for miss.class.train.error of Naive Bayes
## Testing Error 
TestErr <- c(TestErr, mean( predict(mod3,auto.test[,2:6]) != auto.test$mpg01))
TestErr
##  0.1282051 for miss.class.test.error of Naive Bayes 
table(predict(mod3,auto.test[,2:6]),auto.test$mpg01)


## Method 4: Logistic Regression
## Both R code lead to the same results, the default link for binomial is "logit"
glm_model <- glm(mpg01 ~ ., data = auto.train, family = binomial(link = "logit"))
summary(glm_model)
probs <- predict(glm_model, auto.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs >= 0.5] <- 1
table(pred.glm, auto.test$mpg01)
mean(pred.glm != auto.test$mpg01)
## 0.1538462 if we used cylinders, weight, horsepower, displacement, and origin as the only predictors.

## Method 5: KNN model with several K values using only variables that most associated with mpg01.
library(class)
TEALL<-NULL 
xnew <- auto.test[,-1];
for(i in seq(1,16,2)){
  ypred.test <- knn(auto.train[,-1], xnew, auto.train[,1],k=i);
  temptesterror <- mean(ypred.test != auto.test[,1]);
  TEALL <- c(TEALL, temptesterror);
}
output<-data.frame(k_value=c("1","3","5","7","9","11","13","15"),value=TEALL,stringsAsFactors = F)
output
##Optimal K Value
output$k_value[which(output$value==min(output$value))]
# 5


## Cross Validation for all Models ##
set.seed(888)
B= 100; ### number of loops
TEALL = NULL; ### Final TE values
KNNALL= NULL; ### Final Knn values
te1<-NULL;te2<-NULL;te3<-NULL; te4<-NULL;
for (b in 1:B){
  flag = sort(sample(1:n, n1));
  auto.train = Auto[-flag,] 
  auto.test = Auto[flag,]
  auto.train = auto.train[c("mpg01","cylinders","horsepower","weight","displacement","origin")]
  auto.test = auto.test[c("mpg01","cylinders", "horsepower","weight","displacement","origin")]
  
  ### Method 1: LDA
  # fit1 <- lda( y ~ ., data= auto.train, CV= TRUE)
  mod1 <- lda(auto.train[,2:6], auto.train[,1]); 
  pred1test <- predict(mod1,auto.test[,2:6])$class; 
  te1 <- c(te1,mean(pred1test != auto.test$mpg01));  
  
  
  ## Method 2: QDA
  mod2 <- qda(auto.train[,2:6], auto.train[,1])
  te2 <- c(te2, mean( predict(mod2,auto.test[,2:6])$class != auto.test$mpg01))

  
  ## Method 3: Naive Bayes
  mod3 <- naiveBayes( auto.train[,2:6], auto.train[,1])
  te3 <- c(te3,  mean( predict(mod3,auto.test[,2:6]) != auto.test$mpg01))
  
  
  ## Method 4: Logistic Regression
  ## Both R code lead to the same results, the default link for binomial is "logit"
  glm_model <- glm(mpg01 ~ ., data = auto.train, family = binomial(link = "logit"))
  probs <- predict(glm_model, auto.test, type = "response")
  pred.glm <- rep(0, length(probs))
  pred.glm[probs >= 0.5] <- 1
  # table(pred.glm, auto.test$mpg01)
  te4<-c(te4,mean(pred.glm != auto.test$mpg01))
  
  
  ## Method 5: KNN model with several K values using only variables that most associated with mpg01.
  cverror<-NULL
  xnew <- auto.test[,-1];
  for(i in seq(1,16,2)){
    ypred.test <- knn(auto.train[,-1], xnew, auto.train[,1],k=i);
    temptesterror <- mean(ypred.test != auto.test[,1]);
    cverror <- c(cverror, temptesterror);
  
  }
  
  KNNALL<-rbind(KNNALL,cbind(cverror[1],cverror[2],cverror[3],cverror[4],cverror[5],cverror[6],cverror[7],cverror[8]))
}
TEALL<-rbind(TEALL,cbind(te1,te2,te3,te4,KNNALL))
TEALL<-data.frame(TEALL)
colnames(TEALL) <- c("LDA", "QDA", "Naive Bayes", "Log Reg.", "KNN1",
                     "KNN3", "KNN5", "KNN7", "KNN9", "KNN11", "KNN13", "KNN15");
apply(TEALL,2,mean)
apply(TEALL,2,var)

