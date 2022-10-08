# Using the same crime data set uscrime.txt as in Question 8.2, apply Principal Component Analysis
# and then create a regression model using the first few principal components. Specify your new model in
# terms of the original variables (not the principal components), and compare its quality to that of your
# solution to Question 8.2. You can use the R function prcomp for PCA. 
# (Note that to first scale the data, you can include scale. = TRUE to scale as 
# part of the PCA function. Don't forget that, to make a prediction for the new city, 
# you'll need to unscale the coefficients (i.e., do the scaling calculation in
# reverse)!)


rm(list = ls()) #Clear the list
library(GGally) #used for ggpairs
set.seed(1) #reproducible values

data <-read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_6/data 9.1/uscrime.txt", header=TRUE, stringsAsFactors = FALSE)

head(data) #quick check

# Perform PCA on scaled attributes
pca <- prcomp(data[,1:15], scale. = TRUE)
summary(pca)

# Useful visualization when deciding how many principal components to choose.

screeplot(pca, type="lines",col="blue")

## Retrieve first 4 PCs  

# From prcomp output
PCs <- pca$x[,1:4]
attributes(pca$x)
pca$x
PCs

# Build linear regression model with the first 4 principal components

PCcrime <- cbind(PCs, data[,16]) #Create new data matrix with first 4 PCs and crime rate

PCcrime

model <- lm(V5~., data = as.data.frame(PCcrime)) #Create regression model on PCcrime

summary(model)


## Multiple R-squared:  0.309,	Adjusted R-squared:  0.243 


## Get coefficients in terms of original data from PCA coefficients

# PCA Coefficients for this linear regression model

beta0 <- model$coefficients[1]
betas <- model$coefficients[2:5]
beta0

# Intercept beta0 is 905

betas

##  PC1   PC2   PC3   PC4 
## 65.2 -70.1  25.2  69.4 

# Transform the PC coefficients into coefficients for the original variables 
pca$rotation[,1:4]
alphas <- pca$rotation[,1:4] %*% betas
t(alphas)

##          M   So   Ed  Po1  Po2  LF   M.F  Pop   NW    U1   U2 Wealth  Ineq Prob  Time
## [1,] -21.3 10.2 14.4 63.5 64.6 -14 -24.4 39.8 15.4 -27.2 1.43   38.6 -27.5  3.3 -6.61

# However, these coefficients listed above are scaled.
# Must convert back to the original data.

# When scaling, this function subtracts the mean and divides by the standard deviation, for each variable.
#
# So, alpha * (x - mean)/sd = originalAlpha * x.
# That means:
# (1) originalAlpha = alpha/sd
# (2) we have to modify the constant term a0 by alpha*mean/sd

originalAlpha <- alphas/sapply(data[,1:15],sd)
originalBeta0 <- beta0 - sum(alphas*sapply(data[,1:15],mean)/sapply(data[,1:15],sd))

# Here are the coefficients for unscaled data:

t(originalAlpha)


originalBeta0

## 1667

# Estimates of the model:

estimates <- as.matrix(data[,1:15]) %*% originalAlpha + originalBeta0
estimates

# Calculate R^2 and Adjusted R^2

SSE = sum((estimates - data[,16])^2)
SStot = sum((data[,16] - mean(data[,16]))^2)
1 - SSE/SStot

## 0.309

R2 <- 1 - SSE/SStot
R2 - (1 - R2)*4/(nrow(data)-4-1)

## 0.243

# The R-squared and Adjusted R-squared are eqaul when using the PCA ranges.

# Compare with the regression model from previous homework

model2 <- lm( Crime ~ ., data = data)
summary(model2)

# This model has R^2 = 0.803 and R^2_adj = 0.708.

# Results suggest that using all the factors peforms better than running the PCA.

# Let's try all possibilities of principle components to double check.

r2 <- numeric(15) # create a vector to store the R-squared values

for (i in 1:15) {
  pclist <- pca$x[,1:i]  # use the first i prinicipal components
  pcc <- cbind(data[,16],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  r2[i] <- 1 - sum(model$residuals^2)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared
}

r2

# This shows that the model is probably overfitted and thus cross validation is needed.
# In the previous homework, cross-validation resulted in a significantly smaller R-squared than the model showed on its training set.

library(DAAG)
# 
# # do 5-fold cross-validation on PCA Models
# 
r2cross <- numeric(15) # create a vector to store the R-squared values

  for (i in 1:15) {
    pclist <- pca$x[,1:i]  # Run all components
    pcc <- cbind(data[,16],pclist)  # generate the data set
    model <- lm(V1~.,data = as.data.frame(pcc)) # fit the model
    c <- cv.lm(as.data.frame(pcc),model,m=5,plotit = FALSE) # Run a cross-validation
    r2cross[i] <- 1 - attr(c,"ms")*nrow(data)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared for each component
  }

r2cross #results

plot(r2cross, xlab = "Principal Component", ylab = "Cross-validated R-squared with this many principal components",
     ylim = c(0,1), type = "b")

#5th principal component looks like a substantial jump so let's try running lm on this one.
pcc <- cbind(data[,16],pca$x[,5])
model <- lm(V1~.,data = as.data.frame(pcc))
summary(model)

# ## Multiple R-squared:  0.336,     Adjusted R-squared:  0.321 

c <- cv.lm(as.data.frame(pcc),model,m=5, plotit = FALSE) # cross-validate
1 - attr(c,"ms")*nrow(data)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared

