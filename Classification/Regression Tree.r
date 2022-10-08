#10.1a
# There are only 47 points so splitting the data would be very difficult. I started by creating a Regression Tree and then saw that the variables actually used in the tree construction were only 4 attributes.
# Then i tried pruning the tree through cross validation to see if the performance would increase but the best fit was to use all the nodes but I provided a sample of what the pruning would look like. 
# Going forward with the unpruned tree, I calculated yhat and SSres to get my R^2. However, the model needs to be cross-validated because results are mostly inflated on training data due to fitting real and random effects.



rm(list = ls())# Start Fresh
library(DAAG) 
library(tree)
library(randomForest)
set.seed(1)

data <- read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_7/data 10.1/uscrime.txt", header=TRUE, stringsAsFactors = FALSE)

head(data)

#Start with creating a Regression Tree.

data_tree <- tree(Crime~., data = data)
summary(data_tree)
# Variables actually used in tree construction:"Po1" "Pop" "LF"  "NW"

data_tree$frame # The view of the tree splitting

#Visualization
plot(data_tree)
text(data_tree)

# Lets see if tree pruning by using cross-validation will increase performance by analyzing terminal nodes.
# Deviance is a quality-of-fit statistic.
# The x-axis represents the number of terminal nodes.

data_cross_validated <- cv.tree(data_tree)
plot(data_cross_validated$size, data_cross_validated$dev, type = "b")

# This plot suggests that we get the best fit using all of the terminal nodes in the tree that we already plotted.

# Limit the # of tree nodes to prune the regression tree.
nodes <- 4
tree_pruned <- prune.tree(data_tree, best = nodes)

plot(tree_pruned) # Plot tree
text(tree_pruned)

# Advance with the unpruned tree
# SSres preparation

yhat <- predict(data_tree)
SSres <- sum((yhat-data$Crime)^2)

plot(data$Crime, yhat) #Predicted vs. Actual
abline(0,1)

plot(data$Crime, scale(yhat - data$Crime)) #Residuals
abline(0,0)

# Calculate R2 for the training

SStot <- sum((data$Crime - mean(data$Crime))^2)
R2 <- 1 - SSres/SStot
R2

# However, we know that the model needs to be cross-validated because results are mostly inflated on training data due to fitting real and random effects.

# Sum of squared errors for each tree size
prune.tree(data_tree)$size
prune.tree(data_tree)$dev

#Sum of squared errors in cross validation
data_cross_validated <- cv.tree(data_tree)
data_cross_validated$size
data_cross_validated$dev

# The errors become a lot larger than before which means it's overfitted.
# Due to not enough data points,this was the best I could do

############10.1b#################
# I first considered the number of predictions to make and then ran the randomForest function. Then I calculated the SSres of the model and plotted the actual vs. predicted and residual values to analyze my data.
# Then I calculated SStot to get my R^2 on the Training data and then compared it to R^2 through cross validation. The R^2 came out a lot more stable and performed better than the other regression tree model.



# Consider the number of predictors and run the randomForest model
number_of_predictions <- 4
randomForest_data <- randomForest(Crime~., data = data, mtry = number_of_predictions, importance = TRUE)
randomForest_data

# Calculate SSres of the random forest model
yhat.rf <- predict(randomForest_data)
SSres <- sum((yhat.rf-data$Crime)^2)

# Plot of actual vs. predicted crime values
plot(data$Crime, yhat.rf)
abline(0,1)

# Plot residuals
plot(data$Crime, scale(yhat.rf - data$Crime))
abline(0,0)

# R^2 on Training Data
SStot <- sum((data$Crime - mean(data$Crime))^2)
R2 <- 1 - SSres/SStot
R2

# R^2 on Cross Validation
SSE <- 0
for (i in 1:nrow(data)) {
  model <- randomForest(Crime~., data = data[-i,], mtry = number_of_predictions, importance = TRUE)
  SSE = SSE + (predict(model,newdata=data[i,]) - data[i,16])^2
}
1 - SSE/SStot # Better than the regression tree model and, removed a lot of overfitting just like it's supposed to.

############10.2#################
#It would be a great time to use Logistic Regression on whether a political candidate for presidency wins an election (current events). The response/outcome would be (0,1) for win or lose.
#Predictors can be the amount of money spent on the campaign, time spent campaigning, type of political party, education level, and gender. 


############10.3 PART 1#################

rm(list=ls())
set.seed(1)

data<-read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_7/data 10.3/germancredit.txt", sep = " ")

head(data)

# Transform response variable to 0's and 1's

data$V21[data$V21==1]<-0
data$V21[data$V21==2]<-1

# Separate 70% training and 30% test/validation data
m <- nrow(data)
train <- sample(1:m, size = round(m*0.7), replace = FALSE)
train_data <- data[train,]
validation_data <- data[-train,]

# Logistic regression model: Use all the available variables

reg = glm(V21 ~.,family=binomial(link = "logit"),data=train_data)
summary(reg)

# Now use significant variables from the first.
reg = glm(V21 ~ V1+V2+V3+V4+V6+V7+V8+V12+V13+V14+V15+V20,family=binomial(link = "logit"),data=train_data)
summary(reg)

# Now use significant variables from the second.
reg = glm(V21 ~ V1+V2+V3+V4+V6+V8+V12+V13+V14+V15+V20,family=binomial(link = "logit"),data=train_data)
summary(reg)

# Bucket between 0 and 1 manually
train_data$V1A12[train_data$V1 == "A12"] <- 1
train_data$V1A12[train_data$V1 != "A12"] <- 0

train_data$V1A13[train_data$V1 == "A13"] <- 1
train_data$V1A13[train_data$V1 != "A13"] <- 0

train_data$V1A14[train_data$V1 == "A14"] <- 1
train_data$V1A14[train_data$V1 != "A14"] <- 0

train_data$V3A32[train_data$V3 == "A32"] <- 1
train_data$V3A32[train_data$V3 != "A32"] <- 0

train_data$V3A34[train_data$V3 == "A34"] <- 1
train_data$V3A34[train_data$V3 != "A34"] <- 0

train_data$V4A41[train_data$V4 == "A41"] <- 1
train_data$V4A41[train_data$V4 != "A41"] <- 0

train_data$V4A410[train_data$V4 == "A410"] <- 1
train_data$V4A410[train_data$V4 != "A410"] <- 0

train_data$V4A43[train_data$V4 == "A43"] <- 1
train_data$V4A43[train_data$V4 != "A43"] <- 0

train_data$V6A65[train_data$V6 == "A65"] <- 1
train_data$V6A65[train_data$V6 != "A65"] <- 0

train_data$V12A124[train_data$V12 == "A124"] <- 1
train_data$V12A124[train_data$V12 != "A124"] <- 0

train_data$V14A143[train_data$V14 == "A143"] <- 1
train_data$V14A143[train_data$V14 != "A143"] <- 0

train_data$V15A152[train_data$V15 == "A152"] <- 1
train_data$V15A152[train_data$V15 != "A152"] <- 0

train_data$V20A202[train_data$V20 == "A202"] <- 1
train_data$V20A202[train_data$V20 != "A202"] <- 0

# Now use significant variables from the third

reg = glm(V21 ~ V1A12 + V1A13 + V1A14 + V2 + V3A32 + V3A34 + V4A41 + V4A410 + V4A43 + V5 + V6A65 + V8 +V12A124+ V14A143+V15A152+V20A202,family=binomial(link = "logit"),data=train_data)
summary(reg)

#Remove V3A32, V5, V12A124
reg = glm(V21 ~ V1A12 + V1A13 + V1A14 + V2+ V3A34 + V4A41 + V4A410 + V4A43 + V6A65 + V8 + V14A143 + V15A152 + V20A202,family=binomial(link = "logit"),data=train_data)
summary(reg)

#Remove v1a12
reg = glm(V21 ~ V1A13 + V1A14 + V2+ V3A34 + V4A41 + V4A410 + V4A43 + V6A65 + V8 + V14A143 + V15A152 + V20A202,family=binomial(link = "logit"),data=train_data)
summary(reg)

#Remove v20a202
reg = glm(V21 ~ V1A13 + V1A14 + V2+ V3A34 + V4A41 + V4A410 + V4A43 + V6A65 + V8 + V14A143 + V15A152,family=binomial(link = "logit"),data=train_data)
summary(reg)

#ADD 0, 1 to the data set

validation_data$V1A13[validation_data$V1 == "A13"] <- 1
validation_data$V1A13[validation_data$V1 != "A13"] <- 0

validation_data$V1A14[validation_data$V1 == "A14"] <- 1
validation_data$V1A14[validation_data$V1 != "A14"] <- 0

validation_data$V3A34[validation_data$V3 == "A34"] <- 1
validation_data$V3A34[validation_data$V3 != "A34"] <- 0

validation_data$V4A41[validation_data$V4 == "A41"] <- 1
validation_data$V4A41[validation_data$V4 != "A41"] <- 0

validation_data$V4A410[validation_data$V4 == "A410"] <- 1
validation_data$V4A410[validation_data$V4 != "A410"] <- 0

validation_data$V4A43[validation_data$V4 == "A43"] <- 1
validation_data$V4A43[validation_data$V4 != "A43"] <- 0

validation_data$V6A65[validation_data$V6 == "A65"] <- 1
validation_data$V6A65[validation_data$V6 != "A65"] <- 0

validation_data$V14A143[validation_data$V14 == "A143"] <- 1
validation_data$V14A143[validation_data$V14 != "A143"] <- 0

validation_data$V15A152[validation_data$V15 == "A152"] <- 1
validation_data$V15A152[validation_data$V15 != "A152"] <- 0

# test the model

y_hat<-predict(reg,validation_data,type = "response")

# y_hat is a vector of fractions.
# Now we can use a threshold to make yes/no decisions,
# and view the confusion matrix.

rounded_y_hat <- as.integer(y_hat > 0.5)

t <- table(rounded_y_hat,validation_data$V21)
t

acc <- (t[1,1] + t[2,2]) / sum(t)
acc #Here is the accuracy value of the model

############10.3 PART 2#################

# Estimate that threshold_probly identifies a bad as good is 5x worse than threshold_probly identifying good as bad.
threshold_prob <- c()
for(i in 1:100)
{
  rounded_y_hat <- as.integer(y_hat > (i/100))  #threshold preds
  
  matrix <-as.matrix(table(rounded_y_hat,validation_data$V21))
  
  if(nrow(matrix)>1) { col1 <- matrix[2,1] } else { col1 <- 0 }
  if(ncol(matrix)>1) { col2 <- matrix[1,2] } else { col2 <- 0 }
  threshold_prob <- c(threshold_prob, col2*5 + col1)
}

plot(c(1:100)/100,threshold_prob,xlab = "Threshold",ylab = "Threshold Probability",main = "Threshold Probability vs Threshold")

which.min(threshold_prob)

#Determine a good threshold probability
#The threshold probability is 9%.

#Thank you for taking the time to read my homework.
