# Question 3.1 Using the same data set (credit_card_data.txt or credit_card_data-headers.txt) as in Question 2.2, use the kknn function to find a good classifier:

# Question 3.1(a) Using cross-validation (do this for the k-nearest-neighbors model; SVM is optional)

rm(list = ls()) # Clear the list
library(kernlab)
library(kknn) 
#load kknn and kernlab libraries
set.seed(25) # per TA Lecture #2

#Read my credit card data
df <- read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_2/data 3.1/credit_card_data-headers.txt", header=TRUE, stringsAsFactors = FALSE) 

#Look at the first rows of my data to see if the data comes out right based on the parameters set in read.table.
head(df)

# Use train.kknn to train the whole data set.
model_loocv <- train.kknn(R1~.,data = df, kmax = 100, scale = TRUE ) #iterates over all rows
model_loocv

#Loops over the number of desired folds to see which fold is the best for a certain K Nearest Neighbor
k=50
model_loocv_acc=rep(0,k)
for (k in 1:k) {
  pred <- as.integer(fitted(model_loocv)[[k]][1:nrow(df)] +.5)
  model_loocv_acc[k] <- sum(pred == df$R1) / nrow(df) *100
}
#When we perform a cross fold loop, the results show that folding 12 times provides the best accuracy of 85.32%
model_loocv_acc
which.max(model_loocv_acc)
max(model_loocv_acc)

#Question 3.1(a)(optional for ksvm)

#Per Professor Sokol (Lecture 3.3), "Rule of thumb is 50-70% Training and then split the rest equally" so I decided to meet it in the middle with 60-20-20 (60% train, 20% validate & test)
spec = c(train = .6, validate = .2, test = .2) #spec means specifications on how i want my data split up.

#Lecture 3.3 - I used the Random approach so i understand that there are possibilites of the data not being separated equally (more early or late data). But i rather used this approach rather than the Rotation approach because I did not want to include bias's (ex. 5 data point rotation with days).
g = sample(cut( # g means grouping based on taking random rows from the total population of rows
  seq(nrow(df)), 
  nrow(df)*cumsum(c(0,spec)),
  labels = names(spec)
))
#split my data tables so that each one lives on its own.
res = split(df, g)
sapply(res, nrow)/nrow(df)
addmargins(prop.table(table(g)))

#Renaming my dataframes
train <- res$train
validate <- res$validate
test <-res$test

#Evidence of distribution of # of rows
nrow(train)
nrow(validate)
nrow(test)

#I used train data for my model and then predicted using my validation data per the TA. 
#Once I know that the validation set provides a good accuracy, I will continue with my test data.
c_values= c( 10 ^(- 6 ), 10 ^(- 4 ), 10 ^(- 2 ), 1 , 10 , 10 ^ 2 , 10 ^ 4 , 10 ^ 6 )
SVM_Accuracy = c()
for(i in 1:length(c_values)){
  model_SVM <- ksvm(as.matrix(train[,1:10]),as.factor(train[,11]),type = "C-svc", kernel = "vanilladot", C =c_values[i], scaled =TRUE)

  SVM_pred <-predict(model_SVM, validate[,1:10])
  SVM_Accuracy[i] <-sum(SVM_pred == validate[,11]) / nrow(validate) * 100
}

#Validation Data Analysis
SVM_Accuracy #List of accuracy per C value
c_for_test <- c_values[which.max(SVM_Accuracy)]
c_for_test
max(SVM_Accuracy) #Semi-Final accuracy percentage was good at 84.7%

#Now that the validation data has been proven to work, I will run this again using the "test set". This will remove any chance that the final % accuracy was inflated by luck (like the "validation set"). Since the "validation set" and "test set" report consistent results compared to each other, i can report the accuracy based on the "test set" to my stakeholders.
c_values= c( 10 ^(- 6 ), 10 ^(- 4 ), 10 ^(- 2 ), 1 , 10 , 10 ^ 2 , 10 ^ 4 , 10 ^ 6 )
SVM_Accuracy = c()
for(i in 1:length(c_values)){
  model_SVM <- ksvm(as.matrix(train[,1:10]),as.factor(train[,11]),type = "C-svc", kernel = "vanilladot", C =c_values[i], scaled =TRUE)

  SVM_pred <-predict(model_SVM, test[,1:10])
  SVM_Accuracy[i] <-sum(SVM_pred == test[,11]) / nrow(test) * 100
}

#Test Data Analysis
SVM_Accuracy #List of accuracy per C value
c_for_test <- c_values[which.max(SVM_Accuracy)]
c_for_test
max(SVM_Accuracy) #Final accuracy percentage

# Question 3.1(b) Split the data into training, validation, and test data sets for kknn.
# I listed different values of k to use for the most optimal k while splitting the data
# By training on the "train set" and testing on the "validation set", we can see if this model provides good accuracy %'s based on each value of k and the quality of the "validation set". If the model provided bad accuracies, I would revisit fixing/replacing a new model and then check again. (iterating process)
k_values = c(5,10,20,25,50,100)
kknn_accuracy=c()
for(n in 1:length(k_values)){
  model <- kknn(R1~., train = train,test=validate, k = k_values[n], scale = TRUE)
  predicted <-as.integer(fitted(model)+.5)
  kknn_accuracy[n] <-sum(predicted == validate$R1)/nrow(validate)*100
}

#Validation Analysis
kknn_accuracy
max(kknn_accuracy)
k_for_test <- k_values[which.max(kknn_accuracy)]
k_for_test

# If the validation model above is consistently good, I can start testing with the "test set". This will remove any chance that the final % accuracy was inflated by luck (like the "validation set"). Since the "validation set" and "test set" report consistent results compared to each other, i can report the accuracy based on the "test set" to my stakeholders.
for(n in 1:length(k_values)){
  model <- kknn(R1~., train = train,test=test, k = k_values[n], scale = TRUE)
  predicted <-as.integer(fitted(model)+.5)
  kknn_accuracy[n] <-sum(predicted == test$R1)/nrow(test)*100
}

#Test Analysis
kknn_accuracy
max(kknn_accuracy)
k_for_test <- k_values[which.max(kknn_accuracy)]
k_for_test


#Question 4.1
# Describe a situation or problem from your job, everyday life, current events, etc., for which a clustering model would be appropriate. List some (up to 5) predictors that you might use.

#At the Jet Propulsion Laboratory (JPL), I work as a Business Analyst for the Mission Systems Engineering Section. Our Techinical Group Supervisors inherently use clustering when they performance coach their groups. Predictors such as education level, years of experience, current position, current level of the employee (1-6), and current level of performance (1-9) based on JPL's matrix of roles & responsibilities, helps the Group Supervisors identify how to speak with each class of employee for efficient/effective clear communication. Clustering the data into 3 groups (Early Career, Mid Career, and Senior hires) can provide strategic input to business managment on whether we need to hire a certain group of employee for balanced diversification. We are currently hiring more Early Career hires now due to the large proportion of Senior hires that are getting ready to retire in the next 5-10 years.


#Question 4.2 Use the R function kmeans to cluster the points as well as possible. Report the best combination of predictors, your suggested value of k, and how well your best clustering predicts flower type.

rm(list = ls())
iris <- read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_2/data 4.2/iris.txt", header=TRUE, stringsAsFactors = FALSE)
iris <- iris
head(iris)
table(iris$Species)

library(ggplot2)
library(factoextra)

#Run the loop to retrieve SCALED Data
iris_scaled_df <- iris 
for (i in 1:4) { iris_scaled_df[,i] <- (iris[,i]-min(iris[,i]))/(max(iris[,i])-min(iris[,i])) }

#Identify which predictors are best suited for further analysis.
ggplot(iris_scaled_df,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
ggplot(iris_scaled_df,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
ggplot(iris_scaled_df,aes(x = Petal.Length, y = Sepal.Width, col= Species)) + geom_point()
ggplot(iris_scaled_df,aes(x = Sepal.Length, y = Petal.Width, col= Species)) + geom_point()
#Best combination of predictors (Petal.Length and Width)
#Based on the charts above, it's apparent that the predictors (Petal.Length and Width) are more distinguishable to identify the flower type. So we will run further analysis based on these columns (,3:4).
#However, if this was Unsupervised learning, I wouldn't have the response values (type of flower) and I would instead run further analysis based on the predictors that provided the smallest total.withinss
center=5 #I ran this with multiple center values and columns (,3:4) always came out the smallest
kmeans(iris_scaled_df[,1:2],nstart = 20,iter.max = 20, centers=center)$tot.withinss
kmeans(iris_scaled_df[,1:3],nstart = 20,iter.max = 20, centers=center)$tot.withinss
kmeans(iris_scaled_df[,1:4],nstart = 20,iter.max = 20, centers=center)$tot.withinss
kmeans(iris_scaled_df[,2:1],nstart = 20,iter.max = 20, centers=center)$tot.withinss
kmeans(iris_scaled_df[,2:3],nstart = 20,iter.max = 20, centers=center)$tot.withinss
kmeans(iris_scaled_df[,2:4],nstart = 20,iter.max = 20, centers=center)$tot.withinss
kmeans(iris_scaled_df[,3:4],nstart = 20,iter.max = 20, centers=center)$tot.withinss #Lowest tot.withinss so I will deep-dive into this kmeans for further analysis.(Same conclusion as analyzing the ggplots)

#The tot.withinss predicts the number of k required. So by looping through different k values, I constructed the Elbow plot to identify the kink in the curve for the most optimal k value based on the "Total Within Sum of Squares" (tot.withinss).
#Elbow Plot preparation
scaled_cut<- rep(0,10)
for (k in 1:10) 
{
  scaled_cut[k] <- kmeans(iris_scaled_df[,3:4],nstart = 20,iter.max = 20, centers=k+1)$tot.withinss
} 
#tot.withinss sum of squares results
scaled_cut
#ggplot
plot(1:10, scaled_cut,type= "b", xlab = "Number of clusters",ylab="Within Sum of Squares", col="red", pch=10, main ="Scaled Plot to determine optimal K")

# It is initially hard to see the optimal k value using the ggplot but using fviz_nbclust makes it more distinguishable based on the same method of "within cluster sums of squares" (wss).
fviz_nbclust(x = iris_scaled_df[,3:4],FUNcluster = kmeans, method = 'wss' )

# The most optimal k shown above was 3 so now i'm using 3 centers to construct my kmeans model.
model <- kmeans(iris_scaled_df[,3:4],3,nstart = 20)
table(model$cluster,iris_scaled_df$Species)
model
fviz_cluster(model, data = iris_scaled_df[,3:4])


#Analysis of Pedal.Length and Pedal. Width Clustering
# (Look at the table values for the numbers below)
# Total Correct points: 50 + 48 + 46= 144
# Total Incorrect points: 2 from versicolor and 4 from virginica = 6
# The model therefore shows a Percentage Accuracy = between_SS / total_SS is 94% and therefore the model is pretty accurate and indicates a good fit.

#Thank you for taking the time to read over my HW2. 
