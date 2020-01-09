#Data Preprocessing
library(MASS)
library(caret)
library(klaR)

#Inporting the dataset
dataset=read.csv('hour.csv')
dataset=dataset[,3:17]



#Taking care of the missing data
dataset$temp=ifelse(is.na(dataset$temp),
                    ave(dataset$temp,FUN=function(x) mean(x,na.rm = TRUE)),
                    dataset$temp)



#Randomly arranging the dataset
set.seed(42)
rows=sample(nrow(dataset))
dataset=dataset[rows,]



#Categorical variable
dataset$season=factor(dataset$season,
                          levels = c('1','2','3','4'),
                          labels=c(1,2,3,4))
dataset$mnth=factor(dataset$mnth,
                          levels = c('1','2','3','4','5', '6','7','8','9','10','11','12'),
                          labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
dataset$yr=factor(dataset$yr,
                          levels = c('0','1'),
                          labels=c(0,1))
dataset$workingday=factor(dataset$workingday,
                          levels = c('0','1'),
                          labels=c(0,1))
dataset$holiday=factor(dataset$holiday,
                          levels = c('0','1'),
                          labels=c(0,1))
dataset$weekday=factor(dataset$weekday,
                    levels = c('0','1','2','3','4','5', '6'),
                    labels=c(0,1,2,3,4,5,6))
dataset$weathersit=factor(dataset$weathersit,
                          levels = c('1','2','3'),
                          labels=c(1,2,3))



#Splitting the data in training and test
library(caTools)
set.seed(42)
split=sample.split(dataset$cnt,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
y_test=test_set[,15]



#Fitting Linear regression
regressor=lm(formula = cnt ~ season+yr+mnth+hr+holiday+weekday+weathersit+temp+hum+windspeed+casual+registered,
             data = training_set)


#Assessing the dataset for the errors
par(mfrow=c(2,2))
plot(regressor)

#1.Non linearity of the dataset :
#The residual vs fitted graph shows no devition. Therfore, the dataset is linear

#2.Non constant varience  of error terms
#The residual vs fitted graph doesnot show increase in error with the increase in predicted value. 
#Therfore, the dataset has no residual error.

#3.Outliers
#The residual vs Fitted graph has outliers at 7326 and 10289.
#Therfore the dataset is succeptable to outliers

#4.High leveredge points
#There are no high leveredge points the the dataset.
#Therfore, the dataset is not succeptable to high leveredge datapoints.



#Predicting the test set result
#predict(regressor,the data we need to predict)
y_pred=predict(regressor, newdata=test_set)



#Calculating the accuracy of the prediction
actuals_preds <- data.frame(cbind(actuals=test_set$cnt, predicteds=y_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 100%
head(actuals_preds)


#Calculating the summary of the dataset
summary(regressor)