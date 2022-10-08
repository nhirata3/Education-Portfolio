# Question 5.1
# Test to see whether there are any outliers in the last column (number of crimes per 100,000 people).

# Dr. Sokol's advice:
# Check to see if the outlier is "Real" or an "Error". 
# If Real, you need to research/investigate to see what caused it and decide whether your model needs to consider it or not.

rm(list = ls()) # Clear the list
library(outliers) 
library(ggplot2)
#load kknn and kernlab libraries
set.seed(25) 

#Read my credit card data
df <- read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_3/data 5.1/uscrime.txt", header=TRUE, stringsAsFactors = FALSE) 

#Look at the first rows of my data to see if the data comes out right based on the parameters set in read.table.
head(df)

summary(df)

# Quantitative Review:
# Check for normality of the crime data since this is an assumption of the Grubbs test by using some visuals.
# Null hypothesis: Data is normally distributed
plot(df$Crime)
plot(rep(0,length(df$Crime)),df$Crime)
plot(df$Crime,df$Pop) #based on this 2 dimensional plot between population and crime, we can see that the smallest population has the highest crime rate. 
boxplot(df$Crime) 
sort(boxplot(df$Crime, plot=FALSE)$out) #based on the boxplot, we can see that there are 3 outliers. But are they really outliers?
qqnorm(df$Crime);qqline(df$Crime) #based on the Q-Q test, the high outliers are key factors of the data not being normally distributed. But we should run a Shapiro test to be completely sure.
hist(df$Crime) #supplemental visual to see how the crime is distributed.
d <- density(df$Crime) 
plot(d, main = "Density Plot")
polygon(d, col="red", border="blue") #supplemental visual to see the density of crime.

# Before running the Grubbs.Test, we should always run a Q-Q Plot and Shapiro analysis to check if the underlying data is normally distributed because Grubbs.Test automatically assumes a normal distribution.
shapiro.test(df$Crime)
# The test rejects the hypothesis of normality when the p-value is less than or equal to 0.05.  Failing the normality test allows us to state with 95% confidence the data does not fit the normal distribution.
# However, lets run the Grubbs Test anyway to see an initial view of what the p-values are per type (11,10, and 10 (opposite)) or respectively (both sides, one side, the other side).

#Null hypothesis: Not both the min and max points are outliers, but one could be.
grubbs.test(df$Crime,type=11)

# #Null hyposthesis: No outlier in one tail or the other.
grubbs.test(df$Crime, type=10)
grubbs.test(df$Crime, type =10, opposite = TRUE) #opposite means the other side of the tail; No outlier in the other side of the tail.

# The alternative hypothesis, which we will conclude if we reject the null hypothesis, is that at the very least that most extreme point is an outlier (statistically).
# the p-values indicate that there is no evidence whatsoever that any of the data are outliers because the p-value is 1 or greatly above .05. If the p-value is greater than the significance level (0.05), the decision is to fail to reject the null hypothesis (meaning there is no outlier).

#Even if we remove the extreme outliers from the initial box plot data set so that Shapiro's p-value is greater 0.05, the Grubbs Test still shows that there are not outliers in the data because each p-value is greater than the significance level (0.05).
df_remove_outliers <-df # replicate dataframe so that it doesn't mess up the original.
crime <- df_remove_outliers[,16]

outliers <- boxplot(crime, plot=FALSE)$out 
print(outliers) #the outliers
df_remove_outliers[which(crime %in% outliers),] #the outlier rows
df_remove_outliers <- df_remove_outliers[-which(crime %in% outliers),] #remove the outlier rows
shapiro.test(crime) #run the shapiro test again

#Null hypothesis: Not both the min and max points are outliers, but one could be.
grubbs.test(crime,type=11)

# #Null hyposthesis: No outlier in one tail.
grubbs.test(crime, type=10)
grubbs.test(crime, type =10, opposite = TRUE) #opposite means the other side of the tail; No outlier in the other side of the tail.

#Another approach (Use a log scale)
#Based on the analysis provided by http://www.statsci.org/data/general/uscrime.html (where we retrieve the uscrime data), it states that, "Crime is slightly better modeled on a log scale".
# So lets take the log scale of crime and see what happens.
df["log_crime"]<-log(df$Crime) # New column for log scale crime.

#The analysis below shows that you can actually transform the data to make it a normal distribution.
boxplot(df$log_crime) #no outliers based on log scaled crime
qqnorm(df$log_crime);qqline(df$log_crime) #based on the Q-Q test, there are no outliers for the log scaled crime. But we should run a Shapiro test to be completely sure.
hist(df$log_crime) #supplemental visual to see how the log scaled crime is distributed.
d <- density(df$log_crime) 
plot(d, main = "Density Plot")
polygon(d, col="red", border="blue") #supplemental visual to see the density of log scaled crime.


# comparison between log scale and original data.
set <-df[order(-df$log_crime, df$Crime),]
head(set[,16:17],1)
tail(set[,16:17],1)

#Null hypothesis: Not both the min and max points are outliers, but one could be.
grubbs.test(df$log_crime,type=11) # 7.597... refers to the highest crime rate and 5.834... refers to the lowest crime rate.

# #Null hyposthesis: No outlier in one tail.
grubbs.test(df$log_crime, type=10)
grubbs.test(df$log_crime, type =10, opposite = TRUE) #opposite means the other side of the tail; No outlier in the other side of the tail.
#Because each grubbs.test has a p-value greater than 0.05, we fail to reject the null hypothesis (meaning there is no outlier)

#Qualitative Review: (Deeper Investigation)
# The smallest population state (300K) had the HIGHEST Crime Rate and the 2nd highest population state came in 2nd.
# This seemed odd (especially when the lowest crime rate state had 2x more population than the highest crime rate state) so I researched crime statistics per state from http://www.disastercenter.com/crime/ from years 1960 to 1965. Please see the attached "US. States Crime Rate per 100,000.xlsx" excel workbook for the analysis.
# I discovered that Nevada and California had roughly the same population range and crime rate statistics compared to the top 2 crime states in our dataset. 
# Even though Nevada is one of the smallest population states, it's crime rate is consistently ranked #1 or #2 against California. I attached a U.S. Crime statistic report for year 1960-1965 w/ conditional formating to easily see how Nevada compares to the other states.
# Because Nevada and California's crime rate stays consistent each year, it should definitely be included in the data set. 
# Dr. Sokol taught us that there are outliers that are "real" (weird but consistent through out time) or an "error" (ex. hitting an extra "0" on your keyboard; mistake). I believe this is a "real" outlier
# Sometimes statistical modeling can only go so far (not everything is black and white) thus thorough research/qualitative analysis must have a hand in providing a complete answer to certain questions. 

 
# Question 6.1
# Describe a situation or problem from your job, everyday life, current events, etc., for which a Change Detection model would be appropriate. Applying the CUSUM technique, how would you choose the critical value and the threshold?
# At the Jet Propulsion Laboratory (JPL), I work as a Business Analyst for the Mission Systems Engineering Section. JPL mainly uses change detection when measuring a satellites/rover's trajectory to make sure it's going the right way.
# We have engineers in Mission Control that receive alerts when a certain project goes offcourse and the alerts they set are based on a change detection model. Based on CUSUM, I would initially choose a critical value based on 1/2 times the standard deviation of the mean and a T-value at 5 times the standard deviation of the mean. However, I would adjust the T-value again based on how sensitive the outcome. For this example, I would want a smaller target value than normal because I would not want to be responsible for crashing a multi-million dollar project (meaning, I'll take the false alarms).


# Quesiton 6.2.1 & 6.2.2
# I tried to attempt answering in R but i wasn't able to figure out how to loop through each column efficiently (The code below only goes up to 1997).
# Therefore, I created an excel workbook that was able to transparently model and present the data. Workbook title is "CUSUM 6.2"
# I provided all my answers for Question 6.2.1 & 6.2.2 in the excel workbook but feel free to look below at my attempt in R.

## Attempt at programming in R.(Used Excel Workbook instead)
rm(list = ls()) # Clear the list
# library(outliers)
# library(ggplot2)
#load kknn and kernlab libraries
set.seed(25)
temps <- read.table("C:/Users/nhirata/Desktop/Georgia Tech/OneDrive - Georgia Institute of Technology/Georgia Tech/ISYE_6501/Week_3/data 6.2/temps.txt", header=TRUE, stringsAsFactors = FALSE)

#######1996#######
# average the temperature for each column year
month_rows <-temps[1:31,]
month_avgs <- colMeans(month_rows[c(2:length(month_rows))], dims=1, na.rm=T)
month_avgs
# compute the mean of the (now averaged) time series
da_mu <- mean(month_avgs)
# compute the difference between the mean of the time series and each "day"
da_minus_mu <- month_avgs[c(1)]-temps$X1996
# set C
C <- 1.95429 #1/2 times standard deviation (found this values in hindsight after excel)
t <- 19.5429 #5 times standard deviation (found this values in hindsight after excel) 
# subtract C from the difference score
damimu_minus_C <- da_minus_mu - C
# create an empty vector for looping
# include an additional zero to help with indexing
cusum <- 0 * damimu_minus_C

# loop through each day, check the cumulative sum, update the 
# index of our accumulator with the appropriate value
#X1996
for (i in 1:length(damimu_minus_C)) 
{
  checker <- cusum[i] + damimu_minus_C[i+1]
  
  ifelse(checker > 0, cusum[i+1] <- checker, cusum[i+1] <- 0) 
}
plot(cusum)
cusum
which(cusum >19.5429) #the first number represents the row number of when the trigger activates based on the T value which would be July 28th.

#######1997#######
month_avgs[c(2)]
da_minus_mu <- month_avgs[c(2)]-temps$X1997
damimu_minus_C <- da_minus_mu - C
cusum <- 0 * damimu_minus_C
for (i in 1:length(damimu_minus_C)) 
{
  checker <- cusum[i] + damimu_minus_C[i+1]
  ifelse(checker > 0, cusum[i+1] <- checker, cusum[i+1] <- 0) 
}
plot(cusum)
cusum
which(cusum >19.5429) # August 2nd

#Thank you for taking the time to look through this code and my Homework assignment. 