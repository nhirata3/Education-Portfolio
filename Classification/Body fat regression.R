fat<-read.table(file="C:/Users/nhirata/Desktop/ISYE7406/Homework2/fat.csv",sep=",",header=TRUE)
#SPLIT DATA INTO TRAINING AND TEST SETS#
n = dim(fat)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
RNGkind(sample.kind = "Rounding")
set.seed(7406); ### set the seed for randomization
flag = sort(sample(1:n, n1));
flag
fat1train = fat[-flag,] 
fat1test = fat[flag,]

########EXPLORATORY DATA ANALYSIS#############

##B##
#Summary Statistics
summary(fat1train)

#Scatter Plots
plot(fat1train$brozek~fat1train$siri,pch=19,col="blue")
par(mfrow=c(3,3))
plot(fat1train$brozek~fat1train$density,pch=19,col="blue") 
plot(fat1train$brozek~fat1train$weight,pch=19,col="blue") 
plot(fat1train$brozek~fat1train$free,pch=19,col="blue") 
plot(fat1train$brozek~fat1train$thigh,pch=19,col="blue")
plot(fat1train$brozek~fat1train$knee,pch=19,col="blue") 
plot(fat1train$brozek~fat1train$biceps,pch=19,col="blue")
plot(fat1train$brozek~fat1train$forearm,pch=19,col="blue")
par(mfrow=c(1,1))
plot(fat1train$brozek~fat1train$height,pch=19,col="blue") #Weird vertical relationship with an outlier

##C##
##Regression (i) - with all predictors
mod1<-lm(brozek~.,data=fat1train)
summary(mod1)
## Model 1: Training error
MSEtrain <- NULL;
MSEtest  <- NULL; 
MSEmod1train <-   mean( (resid(mod1) )^2);
MSEtrain <- c(MSEtrain, MSEmod1train);
MSEtrain #.02930823
# Model 1: testing error 
## The true Y response for the testing subset
ytrue    <- fat1test$brozek; 
pred1a <- predict(mod1, fat1test[,2:18]);
MSEmod1test <-   mean((pred1a - ytrue)^2);
MSEmod1test;
MSEtest <- c(MSEtest, MSEmod1test); 
MSEtest #.008755981

#Regression (ii) - with best subset of k=5 predictor variables
library(leaps)
fat.leaps <- regsubsets(brozek ~ ., data= fat1train, nbest= 100, really.big= TRUE); 
## Record useful information from the output
fat.models <- summary(fat.leaps)$which;
fat.models.size <- as.numeric(attr(fat.models, "dimnames")[[1]]);
fat.models.size
fat.models.rss <- summary(fat.leaps)$rss;

## 2A:  The following are to show the plots of all subset models 
##   and the best subset model for each subset size k 
plot(fat.models.size, fat.models.rss); 

## find the smallest RSS values for each subset size 
fat.models.best.rss <- tapply(fat.models.rss, fat.models.size, min); 
## Also add the results for the only intercept model
fat.model0 <- lm( brozek ~ 1, data = fat1train); 
fat.models.best.rss <- c( sum(resid(fat.model0)^2), fat.models.best.rss); 
## plot all RSS for all subset models and highlight the smallest values 
plot(0:8, fat.models.best.rss, type = "b", col= "red", xlab="Subset Size k", ylab="Residual Sum-of-Square")
points(fat.models.size, fat.models.rss)

# 2B: What is the best subset with k=5
op2 <- which(fat.models.size == 5); 
flag2 <- op2[which.min(fat.models.rss[op2])]; 
op2
flag2
## There are two ways to fit this best subset model with k=5. 

## 2B(i) First, we can manual look at the selected model and fit it.
##      It will not be able to be implemented in cross-validation 
fat.models[flag2,]
model2a <- lm( brozek ~ siri+density+thigh+knee+wrist, data = fat1train);
summary(model2a);

## 2B(ii) Second, we can auto-find the best subset with k=5
##   this way will be useful when doing cross-validation 
mod2selectedmodel <- fat.models[flag2,]; 
mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+"); 
mod2form <- paste ("brozek ~", mod2Xname);
## To auto-fit the best subset model with k=5 to the data
model2 <- lm( as.formula(mod2form), data= fat1train); 
# Model 2: training error 
MSEmod2train <- mean(resid(model2)^2);
## save this training error to the overall training error vector 
MSEtrain <- c(MSEtrain, MSEmod2train);
MSEtrain;
## Check the answer
#0.0293, 0.0314
## Model 2:  testing error 
pred2 <- predict(model2, fat1test[,2:18]);
MSEmod2test <-   mean((pred2 - ytrue)^2);
MSEtest <- c(MSEtest, MSEmod2test);
MSEtest;
## Check the answer
#0.0087, 0.00278

##As compared to the full model #1, the best subset model with K=5
## has a smaller training error (0.00875 vs. 0.0293) 
## and also has a smaller error (.00278 vs. 0.03146)

#Regression (iii) - with variables (stepwise) selected using AIC
model1 <- lm( brozek ~ ., data = fat1train); 
model3  <- step(model1); 
model3
## If you want, you can see the coefficents of model3
round(coef(model3),3)
summary(model3)

## Model 3: training  and  testing errors 
MSEmod3train <- mean(resid(model3)^2);
pred3 <- predict(model3, fat1test[,2:18]);
MSEmod3test <-   mean((pred3 - ytrue)^2);
MSEtrain <- c(MSEtrain, MSEmod3train);
MSEtrain; 
## [1] 0.02930823 0.03146801 0.02945827
MSEtest <- c(MSEtest, MSEmod3test);
## Check your answer 
MSEtest;
## [1] 0.008755981 0.002786218 0.008955971

#Ridge regression (iv)
### We need to call the "MASS" library in R
### 
library(MASS);

## The following R code gives the ridge regression for all penalty function lambda
##  Note that you can change lambda value to other different range/stepwise 
fat.ridge <- lm.ridge( brozek ~ ., data = fat1train, lambda= seq(0,100,0.001));
## 4A. Ridge Regression plot how the \beta coefficients change with \lambda values 
##   Two equivalent ways to plot
plot(fat.ridge) 
### Or "matplot" to plot the columns of one matrix against the columns of another
matplot(fat.ridge$lambda, t(fat.ridge$coef), type="l", lty=1, 
        xlab=expression(lambda), ylab=expression(hat(beta)))

## 4B: We need to select the ridge regression model
##        with the optimal lambda value 
##     There are two ways to do so

## 4B(i) manually find the optimal lambda value
##    but this is infeasible for cross-validation 
select(fat.ridge)
## 
#modified HKB estimator is 0.00836436  
#modified L-W estimator is 0.007640264 
# smallest value of GCV  at 0.003
#
# The output suggests that a good choice is lambda = 0.003, 
abline(v=0.003)
# Compare the coefficients of ridge regression with lambda= 0.003
##  versus the full linear regression model #1 (i.e., with lambda = 0)
fat.ridge$coef[, which(fat.ridge$lambda == 0.003)]
fat.ridge$coef[, which(fat.ridge$lambda == 0)]

## 4B(ii) Auto-find the "index" for the optimal lambda value for Ridge regression 
##        and auto-compute the corresponding testing and testing error 
indexopt <-  which.min(fat.ridge$GCV);  
indexopt
## If you want, the corresponding coefficients with respect to the optimal "index"
##  it is okay not to check it!
fat.ridge$coef[,indexopt]

## However, this coefficeints are for the the scaled/normalized data 
##      instead of original raw data 
## We need to transfer to the original data 
## Y = X \beta + \epsilon, and find the estimated \beta value 
##        for this "optimal" Ridge Regression Model
## For the estimated \beta, we need to sparate \beta_0 (intercept) with other \beta's
ridge.coeffs = fat.ridge$coef[,indexopt]/ fat.ridge$scales;
intercept = -sum( ridge.coeffs  * colMeans(fat1train[,2:18] )  )+ mean(fat1train[,1]);
## If you want to see the coefficients estimated from the Ridge Regression
##   on the original data scale
c(intercept, ridge.coeffs);

## Model 4 (Ridge): training errors 
yhat4train <- as.matrix( fat1train[,2:18]) %*% as.vector(ridge.coeffs) + intercept;
MSEmod4train <- mean((yhat4train - fat1train$brozek)^2); 
MSEtrain <- c(MSEtrain, MSEmod4train); 
MSEtrain
## [1]  0.02930823 0.03146801 0.02945827 0.02930890
## Model 4 (Ridge):  testing errors in the subset "test" 
pred4test <- as.matrix( fat1test[,2:18]) %*% as.vector(ridge.coeffs) + intercept;
MSEmod4test <-  mean((pred4test - ytrue)^2); 
MSEtest <- c(MSEtest, MSEmod4test);
MSEtest;
## [1] 0.008755981 0.002786218 0.008955971 0.008859234

#LASSO (v)
## IMPORTANT: You need to install the R package "lars" beforehand
##
library(lars)
fat.lars <- lars( as.matrix(fat1train[,2:18]), fat1train[,1], type= "lasso", trace= TRUE);

## 5A: some useful plots for LASSO for all penalty parameters \lambda 
plot(fat.lars)

## 5B: choose the optimal \lambda value that minimizes Mellon's Cp criterion 
Cp1  <- summary(fat.lars)$Cp;
index1 <- which.min(Cp1);

## 5B(i) if you want to see the beta coefficient values (except the intercepts)
##   There are three equivalent ways
##    the first two are directly from the lars algorithm
coef(fat.lars)[index1,]
fat.lars$beta[index1,]
##   the third way is to get the coefficients via prediction function 
lasso.lambda <- fat.lars$lambda[index1]
coef.lars1 <- predict(fat.lars, s=lasso.lambda, type="coef", mode="lambda")
coef.lars1$coef
## Can you get the intercept value? 
##  \beta0 = mean(Y) - mean(X)*\beta of training data
##       for all linear models including LASSO
LASSOintercept = mean(fat1train[,1]) -sum( coef.lars1$coef  * colMeans(fat1train[,2:18] ));
c(LASSOintercept, coef.lars1$coef)

## Model 5:  training error for lasso
## 
pred5train  <- predict(fat.lars, as.matrix(fat1train[,2:18]), s=lasso.lambda, type="fit", mode="lambda");
yhat5train <- pred5train$fit; 
MSEmod5train <- mean((yhat5train - fat1train$brozek)^2); 
MSEtrain <- c(MSEtrain, MSEmod5train); 
MSEtrain
# [1] 0.02930823 0.03146801 0.02945827 0.02930890 0.03085618
##
## Model 5:  training error for lasso  
pred5test <- predict(fat.lars, as.matrix(fat1test[,2:18]), s=lasso.lambda, type="fit", mode="lambda");
yhat5test <- pred5test$fit; 
MSEmod5test <- mean( (yhat5test - fat1test$brozek)^2); 
MSEtest <- c(MSEtest, MSEmod5test); 
MSEtest;
## Check your answer:
## [1] 0.008755981 0.002786218 0.008955971 0.008859234 0.003158102

#Principal component regression (vi)
##
## We can either manually conduct PCR by ourselves 
##   or use R package such as "pls" to auto-run PCR for us
##
## For purpose of learning, let us first conduct the manual run of PCR
##  6A: Manual PCR: 
##  6A (i) some fun plots for PCA of training data
trainpca <- prcomp(fat1train[,2:18]);  
##
## 6A(ii)  Examine the square root of eigenvalues
## Most variation in the predictors can be explained 
## in the first a few dimensions
trainpca$sdev
round(trainpca$sdev,2)
### 6A (iii) Eigenvectors are in oj$rotation
### the dim of vectors is 17
###
matplot(2:18, trainpca$rot[,1:3], type ="l", xlab="", ylab="")
matplot(2:18, trainpca$rot[,1:5], type ="l", xlab="", ylab="")
##
## 6A (iv) Choose a number beyond which all e. values are relatively small 
plot(trainpca$sdev,type="l", ylab="SD of PC", xlab="PC number")
##
## 6A (v) An an example, suppose we want to do Regression on the first 4 PCs
## Get Pcs from obj$x
modelpca <- lm(brozek ~ trainpca$x[,1:4], data = fat1train)
##
## 6A (vi) note that this is on the PC space (denote by Z), with model Y= Z\gamma + epsilon
## Since the PCs Z= X U for the original data, this yields to 
## Y= X (U\gamma) + epsilon,
## which is the form Y=X\beta + epsilon in the original data space 
##  with \beta = U \gamma. 
beta.pca <- trainpca$rot[,1:4] %*% modelpca$coef[-1]; 
##
## 6A (vii) as a comparison of \beta for PCA, OLS, Ridge and LASSO
##   without intercepts, all on the original data scale
cbind(beta.pca, coef(model1)[-1], ridge.coeffs, coef.lars1$coef)
##
### 6A(viii) Prediction for PCA
### To do so, we need to first standardize the training or testing data, 
### For any new data X, we need to impose the center as in the training data
###  This requires us to subtract the column mean of training from the test data
xmean <- apply(fat1train[,2:18], 2, mean); 
xtesttransform <- as.matrix(sweep(fat1test[,2:18], 2, xmean)); 
##
## 6A (iX) New testing data X on the four PCs
xtestPC <-  xtesttransform %*% trainpca$rot[,1:4]; 
##
## 6A (X) the Predicted Y
ypred6 <- cbind(1, xtestPC) %*% modelpca$coef;  
## 
## In practice, one must choose the number of PC carefully.
##   Use validation dataset to choose it. Or Use cross-Validation 
##  This can be done use the R package, say "pls"
##  in the "pls", use the K-fold CV -- default; divide the data into K=10 parts 
##
## 6B: auto-run PCR
##
## You need to first install the R package "pls" below
##
library(pls)
## 6B(i): call the pcr function to run the linear regression on all possible # of PCs.
##
fat.pca <- pcr(brozek~., data=fat1train, validation="CV"); 
fat.pca
## 
## 6B(ii) You can have some plots to see the effects on the number of PCs 
validationplot(fat.pca);
summary(fat.pca); 
## The minimum occurs at 17 components
## so for this dataset, maybe we should use full data
##
### 6B(iii) How to auto-select # of components
##     automatically optimazation by PCR based on the cross-validation
##     It chooses the optimal # of components 
ncompopt <- which.min(fat.pca$validation$adj);
ncompopt
## 
## 6B(iv) Training Error with the optimal choice of PCs
ypred6train <- predict(fat.pca, ncomp = ncompopt, newdata = fat1train[2:18]); #???
MSEmod6train <- mean( (ypred6train - fat1train$brozek)^2); 
MSEtrain <- c(MSEtrain, MSEmod6train); 
MSEtrain;
## 6B(v) Testing Error with the optimal choice of PCs
ypred6test <- predict(fat.pca, ncomp = ncompopt, newdata = fat1test[2:18]); #???
MSEmod6test <- mean( (ypred6test - fat1test$brozek)^2);
MSEmod6test
MSEtest <- c(MSEtest, MSEmod6test); 
MSEtest;
## Check your answer:
## [1] 0.008755981 0.002786218 0.008955971 0.008859234 0.003158102 0.008755981
##
## For this specific example, the optimal # of PC
##         ncompopt = 17, which is the full dimension of the original data
##   and thus the PCR reduces to the full model!!!

#Partial least squares (vii)
###
###  The idea is the same as the PCR and can be done by "pls" package
###  You need to call the fuction "plsr"  if you the code standalone 
library(pls)
fat.pls <- plsr(brozek ~ ., data = fat1train, validation="CV");

### 7(i) auto-select the optimal # of components of PLS 
## choose the optimal # of components  
mod7ncompopt <- which.min(fat.pls$validation$adj);
## The opt # of components, it turns out to be 8 for this dataset,
##       and thus PLS also reduces to the full model!!!    

# 7(ii) Training Error with the optimal choice of "mod7ncompopt" 
# note that the prediction is from "prostate.pls" with "mod7ncompopt" 
ypred7train <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1train[2:18]); 
MSEmod7train <- mean( (ypred7train - fat1train$brozek)^2); 
MSEtrain <- c(MSEtrain, MSEmod7train); 
MSEtrain;
## 7(iii) Testing Error with the optimal choice of "mod7ncompopt" 
ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1test[2:18]); 
MSEmod7test <- mean( (ypred7test - fat1test$brozek)^2); 
MSEtest <- c(MSEtest, MSEmod7test); 
MSEtest;

## Check your answers
MSEtrain 
## Training errors of these 7 models/methods
#[1] 0.02930823 0.03146801 0.02945827 0.02930890 0.03085618 0.02930823 0.02930823
## Testing errors of these 7 models/methods
#[1] 0.008755981 0.002786218 0.008955971 0.008859234 0.003158102 0.008755981 0.008755981
##
## For this specific dataset, PCR and PLS reduce to the full model!!!


####CROSS VALIDATION####

### Part (e): the following R code might be useful, and feel free to modify it.
### save the TE values for all models in all $B=100$ loops
B= 100; ### number of loops
TEALL = NULL; ### Final TE values
te1<-NULL;te2<-NULL;te3<-NULL; te4<-NULL; te5<-NULL;te6<-NULL;te7<-NULL;
set.seed(7406); ### set the seed for randomization
for (b in 1:B){
  ### randomly select 25 observations as testing data in each loop
  flag <- sort(sample(1:n, n1))
  # print(flag)
  fattrain <- fat[-flag,];
  fattest <- fat[flag,];
  ##Regression (i) - with all predictors
  mod1<-lm(brozek~.,data=fattrain)
  ## Model 1: Training error
  ## The true Y response for the testing subset
  ytrue    <- fattest$brozek; 
  pred1a <- predict(mod1, fattest[,2:18]);
  MSEmod1test_a <-mean((pred1a - ytrue)^2);
  te1 <- c(te1, MSEmod1test_a); 
  

  ###BEST SUBSET W/ k=5###
  fat.leaps <- regsubsets(brozek ~ ., data= fattrain, nbest= 100, really.big= TRUE);
  ## Record useful information from the output
  fat.models <- summary(fat.leaps)$which;
  fat.models.size <- as.numeric(attr(fat.models, "dimnames")[[1]]);
  fat.models.rss <- summary(fat.leaps)$rss;
  op2 <- which(fat.models.size == 5);
  flag2 <- op2[which.min(fat.models.rss[op2])];
  ## 2B(ii) Second, we can auto-find the best subset with k=5
  ##   this way will be useful when doing cross-validation
  mod2selectedmodel <- fat.models[flag2,];
  mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+");
  mod2form <- paste ("brozek ~", mod2Xname);
  ## To auto-fit the best subset model with k=5 to the data
  model2 <- lm( as.formula(mod2form), data= fattrain);
  pred2 <- predict(model2, fattest[,2:18]);
  MSEmod2test_a <-mean((pred2 - ytrue)^2);
  te2 <- c(te2, MSEmod2test_a);

  ###Stepwise###
  #Regression (iii) - with variables (stepwise) selected using AIC
  model1 <- lm( brozek ~ ., data = fattrain);
  model3  <- step(model1,trace=0);
  ## Model 3: training  and  testing errors
  MSEmod3train <- mean(resid(model3)^2);
  pred3 <- predict(model3, fattest[,2:18]);
  MSEmod3test_a <-mean((pred3 - ytrue)^2);
  te3 <- c(te3, MSEmod3test_a);

  ###RIDGE REGRESSION###
  ## The following R code gives the ridge regression for all penalty function lambda
  ##  Note that you can change lambda value to other different range/stepwise
  fat.ridge <- lm.ridge( brozek ~ ., data = fattrain, lambda= seq(0,100,0.001));

  ## 4B(ii) Auto-find the "index" for the optimal lambda value for Ridge regression
  ##        and auto-compute the corresponding testing and testing error
  indexopt <-  which.min(fat.ridge$GCV);

  ## For the estimated \beta, we need to separate \beta_0 (intercept) with other \beta's
  ridge.coeffs = fat.ridge$coef[,indexopt]/ fat.ridge$scales;
  intercept = -sum( ridge.coeffs  * colMeans(fattrain[,2:18] )  )+ mean(fattrain[,1]);

  ## Model 4 (Ridge): training errors
  yhat4train <- as.matrix( fattrain[,2:18]) %*% as.vector(ridge.coeffs) + intercept;
  MSEmod4train <- mean((yhat4train - fattrain$brozek)^2);
  ## Model 4 (Ridge):  testing errors in the subset "test"
  pred4test <- as.matrix( fattest[,2:18]) %*% as.vector(ridge.coeffs) + intercept;
  MSEmod4test_a <-  mean((pred4test - ytrue)^2);
  te4 <- c(te4, MSEmod4test_a);

  ###LASSO###
  fat.lars <- lars( as.matrix(fattrain[,2:18]), fattrain[,1], type= "lasso", trace= FALSE);

  ## 5B: choose the optimal \lambda value that minimizes Mellon's Cp criterion
  Cp1  <- summary(fat.lars)$Cp;
  index1 <- which.min(Cp1);
  # print(which.min(Cp1))
  ##   the third way is to get the coefficients via prediction function
  lasso.lambda <- fat.lars$lambda[index1]
  coef.lars1 <- predict(fat.lars, s=lasso.lambda, type="coef", mode="lambda")
  coef.lars1$coef
  ## Can you get the intercept value?
  ##  \beta0 = mean(Y) - mean(X)*\beta of training data
  ##       for all linear models including LASSO
  LASSOintercept = mean(fattrain[,1]) -sum( coef.lars1$coef  * colMeans(fattrain[,2:18] ));
  c(LASSOintercept, coef.lars1$coef)

  ## Model 5:  training error for lasso
  pred5test <- predict(fat.lars, as.matrix(fattest[,2:18]), s=lasso.lambda, type="fit", mode="lambda");
  yhat5test <- pred5test$fit;
  MSEmod5test_a <- mean( (yhat5test - fattest$brozek)^2);
  te5 <- c(te5, MSEmod5test_a);
  
  ###PCA###
  fat.pca <- pcr(brozek~., data=fattrain, validation="CV");
  ncompopt <- which.min(fat.pca$validation$adj);
  ncompopt
  ## 6B(v) Testing Error with the optimal choice of PCs
  ypred6test <- predict(fat.pca, ncomp = ncompopt, newdata = fattest[2:18]); #???
  MSEmod6test_a<-(mean( (ypred6test - fattest$brozek)^2));
  te6<-c(te6,MSEmod6test_a)

  fat.pls <- plsr(brozek ~ ., data = fattrain, validation="CV");

  ## 7(i) auto-select the optimal # of components of PLS
  # choose the optimal # of components
  mod7ncompopt <- which.min(fat.pls$validation$adj);
  ## 7(iii) Testing Error with the optimal choice of "mod7ncompopt"
  ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fattest[2:18]);
  MSEmod7test <- mean( (ypred7test - fattest$brozek)^2);
  te7 <- c(te7, MSEmod7test);
  te7
  
  
} 
TEALL = rbind(TEALL,cbind(te1,te2,te3,te4,te5,te6,te7))
mean(te1)
mean(te2)
mean(te3)
mean(te4)
mean(te5)
mean(te6)
mean(te7)

dim(TEALL)
colnames(TEALL)<-c("mod1","mod2","mod3","mod4","mod5","mod6","mod7")
apply(TEALL,2,mean)
apply(TEALL,2,var)

