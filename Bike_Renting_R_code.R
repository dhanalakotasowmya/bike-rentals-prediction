#remove all program/objects from RAM
rm(list=ls())

#set Working directory
setwd("D:/Bike Renting")

#cheking current working directory
getwd()

#before starting this project with dataset.
#Initially I open given file in excel and just look the data available
#starting the project We need to load some libraries to deal with this data
install.packages(c("lattice", "dplyr", "plyr", "ggplot2", "corrgram"))
install.packages("contrib.url")

#install required libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("corrgram")
library("lattice")

#load required dataset
data = read.csv("day.csv", header = T)

###################################################################################################
# Understanding the data
##################################################################################################
#after loading the dataset
#let's check the number of variables and obseravtions in dataset
dim(data)
#we see that there are 731 observations(Rows) and 16 variables(Columns) in given dataset.

#checking first few rows of data
head(data,10)
#in this "cnt" is our Target Variable and others are predictor variables

#check the summary of the data
summary(data)

#it shows that varable like "yr", "mnth", "holiday", "weekday", "workingday", "weathershit" 
#are categorical variables and already encoded.

#numerical variables like "temp", "atemp", "hum", windspeed" are already converted into 
#noramlized form as that mentioned into problem statement.

#cheking data structure
str(data)

#as categorical variables shows the int datatypes. So we need to convert them into proper datatype
#i.e. factor variable

#changing dtype of categotical variables
data$season = as.factor(data$season)
data$yr = as.factor(data$yr)
data$mnth =  as.factor(data$mnth)
data$holiday =  as.factor(data$holiday)
data$weekday =  as.factor(data$weekday)
data$workingday =  as.factor(data$workingday)
data$weathersit = as.factor(data$weathersit)

#checking data structure again
str(data)

#in this dataset, some of the variables are not useful for further analysis for that reason 
#we going to dropped that variables 
######### Dropping Variables which are not required
############## instant: index number which is not useful in analysis
############## dteday: all required parameters are already extracted from this variable such as 
####################### yr, mth, weekday. So this variable is not useful

#dropping instant & dteday variables 
data = select(data, -c(instant, dteday))

#check dataset after dropping variables
dim(data)
head(data,10)

###################################################################################################
#Missing Value Analysis
##################################################################################################
#checking number of missing values in dataset
sum(is.na(data))

#checking variables with missing values count
sapply(data, function(x) sum(is.na(x)))

#there is no missing values in dataset

##################################################################################################
#Outlier Analysis
##################################################################################################
#here, we use Boxplot Method to visualize the ouliers in our dataset

#plotting boxplot for "cnt" variable 
boxplot(data$cnt, data=data, main = "Boxplot for cnt")

#after checking the Boxplot for cnt, it is evident that there is no outlier present in cnt

#let's check the outliers of predictor variables such as temp, atemp, hum, winspeed, casual, registered

#plotting boxplot for "temp" variable
boxplot(data$temp, data=data, main = "Boxplot for temp")
#no outliers in temp variable

#plotting boxplot for "atemp" variable
boxplot(data$atemp, data=data, main = "Boxplot for atemp")
#no outliers in atemp variable

#plotting boxplot for "hum" variable
boxplot(data$hum, data=data, main = "Boxplot for hum")
#outliers preset in hum variable, that we will remove after checking all variables ouliter

#plotting boxplot for "windspeed" variable
boxplot(data$windspeed, data=data, main = "Boxplot for windspeed")
#outliers preset in windspeed variable, that we will remove after checking all variables ouliter

#plotting boxplot for "causal" variable
boxplot(data$casual, data=data, main = "Boxplot for casual")
#outliers preset in casual variable, that we will remove after checking all variables ouliter

#plotting boxplot for "registered" variable
boxplot(data$registered, data=data, main = "Boxplot for registered")
#no outliers in registered variable

#as we see there are some outliers present in "hum", "windspeed" and "casual" variables
#varaibles hum, windspeed, casual has ouliers and that have to be remove by 
#outlier removal method 

#make copy of data if anything wrong goes
df = data
data =df
########### Treating Outliers ##############################
#findout the numeric variables in dataset
numeric_index = sapply(data, is.numeric)

#prepare the numeric dataset 
numeric_data = data[, numeric_index]

cnames = colnames(numeric_data)

#loop to remove outliers from all variables
for(i in cnames){
  qnt = quantile(data[i], probs = c(.75, .25), na.rm = T)
  iqr = qnt[1] - qnt[2]
  min = qnt[2] - (iqr*1.5)
  max = qnt[1] + (iqr*1.5)
  print(min)
  print(max)
  data = subset(data, data[i]>min)
  data = subset(data, data[i]<max)
}

#there are 55 observations are dropped in outliers

#################################################################################################
#correlation Analysis
#################################################################################################
#Correlation Analysis: Here we generating correlation matrix to understand 
#how the each variable related with each other. In that we plotting correlation matrix 
#and generate plot using corrgram library for better understanding

#this metrix wil be plot only using numeric data for that we provide numeric data
corr = cor(numeric_data)
print(corr)

#plotting correlation plot using corrgram library
corrgram(numeric_data, order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "Correlation Plot")

#The above correlation analysis shows that,
#temp and atemp are highly correlated
#temp and atemp have positive and strong correlation with cnt
#hum and windspeed have negative and weak correlation with cnt

#dropping atemp variable from a dataset
data = select(data, -c(atemp))

#################################################################################################
# Exploratory Data Analysis
#################################################################################################

################################ Bivariate Analysis ############################################
#finding the relationship between Numerical variables "temp", "hum", "windspeed" with 
#target variable cnt

#relation between cnt vs temp
attach(data)
plot(temp, cnt, main="Regression Plot of cnt vs temp",
     xlab="temp", ylab="cnt") 
abline(lm(cnt~temp), col="blue") # regression line (y~x) 

#relation between cnt vs hum
plot(hum, cnt, main="Regression Plot of cnt vs hum",
     xlab="hum", ylab="cnt") 
abline(lm(cnt~hum), col="red") # regression line (y~x)

#relation between cnt vs windspeed
plot(windspeed, cnt, main="Regression Plot of cnt vs windspeed",
     xlab="temp", ylab="cnt") 
abline(lm(cnt~windspeed), col="green") # regression line (y~x)

#after plotting above graph, we see that
### cnt has positive linear relationship with temp
### on the otherside, cnt has negative linear relationship with windspeed
### but hum(humidity) has a little neagive relationship with cnt

#now let's find the relationship between categorical variables and Target Variables "cnt"
#categorical variables are "season", "holiday", "weekday", "Workingday", "weathershit", "month"

#plotting boxplot for cnt vs season variable
ggplot(data, aes(x=season, y=cnt)) + 
  geom_boxplot()+xlab("season")+ylab("cnt")+ggtitle("Boxplot of cnt vs season")
##cnt is very low in Spring Season and cnt is large in Fall Season 

#plotting boxplot of cnt vs holiday
ggplot(data, aes(x=holiday, y=cnt)) + 
  geom_boxplot()+xlab("holiday")+ylab("cnt")+ggtitle("Boxplot of cnt vs holiday")
##cnt is more on weekday i.e. no holiday

#plotting boxplot of cnt vs weekday
ggplot(data, aes(x=weekday, y=cnt)) + 
  geom_boxplot()+xlab("weekday")+ylab("cnt")+ggtitle("Boxplot of cnt vs weekday")
#as per the graph more number of bikes are used on Friday

#plotting boxplot of cnt vs workingday
ggplot(data, aes(x=workingday, y=cnt)) + 
  geom_boxplot()+xlab("workingday")+ylab("cnt")+ggtitle("Boxplot of cnt vs workingday")
#as per the graph more number of bikes are used on Workingday, 
#this conclusion we already get from boxplot of cnt vs holiday

#plotting boxplot of cnt vs month
ggplot(data, aes(x=mnth, y=cnt)) + 
  geom_boxplot()+xlab("month")+ylab("cnt")+ggtitle("Boxplot of cnt vs month")
#as per the graph more number of bikes are used in July Month of year

#plotting boxplot of cnt vs weathersit
ggplot(data, aes(x=weathersit, y=cnt)) + 
  geom_boxplot()+xlab("weathersit")+ylab("cnt")+ggtitle("Boxplot of cnt vs weathersit")
#1 >>as per the graph more number of bikes are used when weather condition is Clear, Few clouds, 
#Party cloudy, Partly cloudy
#2 >> and less number of bikes are used when weather condition is 
#Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#3 >> No bikes where used when weather condition is 
#Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

##########Summary#############
## cnt is maximum in good weather condition and minimum in bad weather condition
## more number of bikes are rented on weekdays.

##################################################################################################
#Feature Scalling
##################################################################################################
#as we know that, most of given variables are already Normalised
#But let's clarify that using Visualization techniques

#check whether variable "temp" is normal or not
plot(density(data$temp), main="Normalization of temp")
polygon(density(data$temp), col="red", border="blue") 

#check whether variable "hum" is normal or not
plot(density(data$hum), main="Normalization of hum")
polygon(density(data$hum), col="red", border="blue") 

#check whether variable "windspeed" is normal or not
plot(density(data$windspeed), main="Normalization of windspeed")
polygon(density(data$windspeed), col="red", border="blue") 

#check whether variable "casual" is normal or not
plot(density(data$casual), main="Normalization of casual")
polygon(density(data$casual), col="red", border="blue") 

#checking whether varianle "registered" is normal or not
plot(density(data$registered), main="Normalization of registered")
polygon(density(data$registered), col="red", border="blue") 

#checking whether variable "cnt" is normal or not
plot(density(data$cnt), main="Normalization of cnt")
polygon(density(data$cnt), col="red", border="blue")

#As we can see, our data is in proper scalling form. Numerical Predictor Variables are Normalized
#Also our Target Variable 'cnt' is also close to Normal Distribution

#################################################################################################
#Modeling & Prediction
#################################################################################################
##Data Cleaning is done! Now lets bulid the model and predict the results

#In machine Learning there is Two main types:

#Supervised Machine Learning : knowledge of output. Target Variable is fix
#Unsupervised Machine Learning: No knowledge of Output. Self Guided Learnig Algorithms.

#Selecting model is main Part of Modelling, We have various model algorithms some of 
#the basic algorithms are:

#Linear Regression : Best suitable for Regression Model
#Logistic Regression: Suitable for Classification Model
#Decision Tree: Best suitable for Regression & Classification model
#Random Forest: Mostly used for Classification model analysis but can be use for Regression model
#KNN algorithms: Can be used for Regression and Classification model
#Naive Bayes: used for Classification Model

#in our given dataset, Target Variable "cnt" is Numerical Continuous Variable. So we are dealing 
#with Regression Model Analysis.

#for that reason we are considering following algorithms
#1 >> Linear Regression
#2 >> Decision Tree
#3 >> Random Forest
#4 >> KNN Algorithms

#before moving further let's droped the casual and registered variables because there sum is 
#equal to target variable i.e. "cnt"
data = select(data, -c(casual, registered))

#Now to predict the model, to simulate a train and test set, we are going to split randomly 
#this train dataset into 80% train and 20% test.

df1 = data
#Random Sample indexes
train_index = sample(1:nrow(data), 0.8*nrow(data))
test_index = setdiff(1:nrow(data), train_index)

#Build train_X, train_y, test_X, test_y
train_X = data[train_index, -11]
train_y = data[train_index, "cnt"]

test_X = data[test_index, -11]
test_y = data[test_index, "cnt"]

#################################################################################################
#Linear Regression Model
#################################################################################################
install.packages("usdm")
library(usdm)
model_LR = lm(train_y ~ ., data=train_X)

summary(model_LR)

#predict the output for test_X dataset
predict_LR = predict(model_LR, test_X) 

#A few things, we learn from this output
#season, yr, weekday, workingday, weathersit, temp have small p-values, 
#where as mnth, holiday, hum, windspeed have a larger p-values
#Here we reject the null-hypothesis for season, yr, weekday, workingday, weathersit, temp
#There is assicoation between these variables and Target Variable cnt

#Fail to reject the null hypothesis for mnth, holiday, hum, windspeed
#There is no association between these variables and Target Variable cnt

#R-squared (0.8652) means this model provides best fit for the given data 
#but Selecting the model with the highest R-squared is not a 
#reliable approach for choosing the best linear model.

#####Better Solution is###########
#Do model Evaluation based on the Error Metrics for Regression:

#For classification problems, we have only used classification accuracy as our evaluation metric.
#But here we used Error Metrics to evaluate the model

#Mean Absolute Error (MAE): is the mean of the absolute value of the errors: 
#In [0,???), the smaller the better

#Mean Squared Error (MSE): is the mean of the squared errors: In [0,???), the smaller the better

#Mean Absolute Percent Error (MAPE): is the mean of the absolute percent value of the errors: 
#In [0,1), the smaller the better

#Root Mean Squared Error (RMSE) :is the square root of the mean of the squared errors: 
#In [0,???), the smaller the better

#Let's calculate these by hand, to get an intuitive sense for the results:
install.packages("Metrics")

library(Metrics)

# calculate MAE, MSE, MAPE, RMSE
mae(test_y, predict_LR) #503.6793
mse(test_y, predict_LR) #473119.3
mape(test_y, predict_LR) #0.1684646
rmse(test_y, predict_LR) #687.8367

####
#MAE gives less weight to outliers means it is not sensitive to outliers.
#MAPE is similar to MAE, but normalized the true obeservations. When true observation is zero 
#then this metric will be problematic
#MSE is a combination measurement of bias and variance of predictions. It is more popular.
#RSME is square Root of MSE, Root square is taken to make the units of the error be the same as 
#the units of the target. This measure gives more weight to large deviations such as outliers, 
#since large differences squared become larger and small (smaller than 1) 
#differences squared become smaller.

#Selection: Outoff these 4 error metrics, MSE and RMSE are mainly used for Time-Series dataset. 
#As I know, current working data is not a time dependend or time-series data.

#for that Reason the Model Evaluation is based on MAPE Error Metrics

################################################################################################
#Decision Tree
################################################################################################
#load libraries
library(rpart)
library(MASS)

#use rpart for Decision Tree regression 
model_DT = rpart(train_y ~ ., data=train_X, method = "anova")

summary(model_DT)

#predict the output for test_X dataset
predict_DT = predict(model_DT, test_X) 

# calculate MAE, MSE, MAPE, RMSE
mae(test_y, predict_DT) #652.6066
mse(test_y, predict_DT) #722906.5
mape(test_y, predict_DT) #0.2316771
rmse(test_y, predict_DT) #850.2391

################################################################################################
#Random Forest
################################################################################################
#load libraries
library(randomForest)

#use randomForest for Random Forest regression 
model_RF = randomForest(train_y ~ ., data=train_X, importance = TRUE, ntree = 50)

summary(model_RF)

#predict the output for test_X dataset
predict_RF = predict(model_RF, test_X) 

# calculate MAE, MSE, MAPE, RMSE
mae(test_y, predict_RF) #295.5393
mse(test_y, predict_RF) #207524.9
mape(test_y, predict_RF) #0.1258508
rmse(test_y, predict_RF) #455.549

################################################################################################
#KNN Algorithms
################################################################################################
#load library
library(caret)

#use knnreg for KNN regression algorithms
model_KNN = knnreg(train_y ~.,data=train_X, k=5)

summary(model_KNN)

#predict the output for test_X dataset
predict_KNN = predict(model_KNN, test_X) 

# calculate MAE, MSE, MAPE, RMSE
mae(test_y, predict_KNN) #735.0471
mse(test_y, predict_KNN) #866782.7
mape(test_y, predict_KNN) #0.2524476
rmse(test_y, predict_KNN) #931.0117

#################################################################################################
#Selecting Best Fit model for Future Analysis
################################################################################################

#We are cosidering the MAPE for model evaluation becasue, 
#it calculate average absolute percent error for 
#each time period minus actual values divided by actual values.

#Reason we already discussed, let's explain again:

####Selection: Outoff these 4 error metrics, 
#MSE and RMSE are mainly used for Time-Series dataset. As we know, 
#current working data is not a time dependend or time-series data.

#Random Forest Model has smallest error metrics i.e.
# MAPE = 0.1258508

#So, for further analysis We are selecting Random Forest Model.