
rm(list = ls())
set.seed(1)
data <- read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_10/data 14.1/breast-cancer-wisconsin.data.txt", stringsAsFactors = FALSE, header = FALSE, sep = ",")

head(data)

# Find where the missing data is.

for (i in 2:11) {
  print(paste0("V",i))
  print(table(data[,i]))
}

# Factor V7 has missing values which shows as ?

# Show the observations with missing data.

data[which(data$V7 == "?"),]


# Calculate % of missing that to make sure it doesn't go over 5% per factor.

nrow(data[which(data$V7 == "?"),])/nrow(data)

## 0.0229

# Less than 5% so we can proceed.

# Locate the row #'s that have missing values in V7.

missing <- which(data$V7 == "?", arr.ind = TRUE)
missing

## 24  41 140 146 159 165 236 250 276 293 295 298 316 322 412 618


### Mean/Mode imputation Method

# Use mode imputation since V7 is categorical
mode <- function(v) {
  value <- unique(v)
  value[which.max(tabulate(match(v, value)))]
}

V7_mode <- as.numeric(mode(data[-missing,"V7"]))
V7_mode # the mode for V7

## 1

# Impute V7 based on V7_mode

imputate_V7 <- data
imputate_V7[missing,]$V7 <- V7_mode
imputate_V7$V7 <- as.integer(imputate_V7$V7)


### Regression Imputation

# Remove response/outcome variable

data_rev1 <- data[-missing,2:10]
data_rev1$V7 <- as.integer(data_rev1$V7)

# Linear model for V7

model <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10, data = data_rev1)
summary(model)

# Use Backward Selection and run again with only significator factors.

step(model)

# Generate the linear model that backward selection recommends.

model2 <- lm(V7~V2+V4+V5+V8, data = data_rev1)
summary(model2)

# All predictors are now significant.

# Now cross-validate the model to test the true R2

library(DAAG)
cv_model <- cv.lm(data_rev1, model2, m=5, plotit = FALSE)
SST <- sum((as.numeric(data[-missing,]$V7) - mean(as.numeric(data[-missing,]$V7)))^2)
cv_rsquared <- 1 - attr(cv_model,"ms")*nrow(data[-missing,])/SST
cv_rsquared

## 0.608

# Get predictions for missing V7 values.

V7_hat <- predict(model2, newdata = data[missing,])

# Use predicted values to impute V7 for data points with missing data

data_rev2 <- data
data_rev2[missing,]$V7 <- V7_hat
data_rev2$V7 <- as.numeric(data_rev2$V7)
data_rev2[missing,]$V7 <- round(V7_hat)
data_rev2$V7 <- as.integer(data_rev2$V7) # Convert to Integers

# No V7 values are allowed to be outside the original range.
data_rev2$V7[data_rev2$V7 > 10] <- 10
data_rev2$V7[data_rev2$V7 < 1] <- 1

### Regression Imputation w/ Pertubation

set.seed(1)

# Perturbate missing V7 value predictions with a random normal distriubtion where the means are the predictions and the standard deviation of the predictions is the standard deviation.

V7_hat_pert <- rnorm(nrow(data[missing,]), V7_hat, sd(V7_hat))
V7_hat_pert

## 4.078  8.386 -0.855  5.138  1.707  0.407  3.790  3.391  3.343  5.413  4.320  3.386  3.875 -3.118  3.467  0.564

data_rev3 <- data
data_rev3[missing,]$V7 <- V7_hat_pert
data_rev3$V7 <- as.numeric(data_rev3$V7)

data_rev3[missing,]$V7 <- round(V7_hat_pert)
data_rev3$V7 <- as.integer(data_rev3$V7) # Convert to Integers

# No V7 values are allowed to be outside the original range.

data_rev3$V7[data_rev3$V7 > 10] <- 10
data_rev3$V7[data_rev3$V7 < 1] <- 1

### Question 15.1
## Describe a situation or problem from your job, everyday life, current events, etc., for which optimization would be appropriate. 
## What data would you need?

# Creating a optimization model for a political campaign schedule would be a perfect way to see how many additional votes the candidate can get.
# Data such as time spent in a particular state, # of visits of that state, if the candidate went to that state, and how long a candidate spent campaigning in that state.
# The constraints can be that the candidate has 20 days left to campaign, must visit Georgia at least 4 times, and must spend at least 1 day in a particular state.
# The Objective Function (how many additional votes) can then be derived from the equation and would provide a significant edge to the candidate against his competition.


