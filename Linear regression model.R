#install Libraries

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(stats4)
library(reshape2)
library(stringr)
library(Metrics)



#Loading the data
weather<-read.csv("/home/rugt69/R practice/Summary of Weather.csv")
#Q1
#Show all the columns in the dataframe 
#Show average of max temp and min temp 
#Show summary statistics median , min , max of min temp and max temp
#Calc pearson and spearman correaltion for min and max temp . Tell what you interpret 
#Plot scatter plot between min and max temp

# Selecting only the required columns (Max,Min Temp) Method 1
corrected_weather<- select(weather, MaxTemp, MinTemp)
summary(corrected_weather)
# Method 2
t<-weather%>% select(MaxTemp, MinTemp)%>%mutate(diff=MaxTemp-MinTemp)%>% summary()
# Calculating the pearson correlation coefficient
cor(corrected_weather$MaxTemp, corrected_weather$MinTemp, method = "pearson")
#It has a positive linear relationship
#Calculating the spearman coeffiient
cor(corrected_weather$MaxTemp, corrected_weather$MinTemp, method = "spearman")
#It also has positive linear relationship
#Scatter plot
ggplot(corrected_weather, aes(MaxTemp, MinTemp)) +
  geom_point() 


#Q2
#Linear regression to predict min temperature using maximum temperature 
#Interpret  the coefficients , p value
#Show QQ plot , goodness of fit test

#Checking the correlation
cor(corrected_weather)
# Linear regression model
weather_model <- lm(formula = MinTemp~MaxTemp, data=corrected_weather) #(y=mx+c)
summary(weather_model)
# The p-value is 2.2e-16 which is below 0.05 so we cannot reject null hypothesis
#Adding the predicted min temp to our dataframe
corrected_weather$predicted_min_temp=weather_model$fitted.values

fitted(corrected_weather)
corrected_weather$residuals=weather_model$residuals
File_2$residuals=Linear_model$residuals
#
#Calculating the RMSE (Root Mean Square Error)
sqrt(mean((corrected_weather$MinTemp - corrected_weather$predicted_min_temp)^2))
rmse(corrected_weather$MinTemp, corrected_weather$predicted_min_temp)

#Calculating MAE (Mean absolute error)
mae(corrected_weather$MinTemp, corrected_weather$predicted_min_temp)


#Accuracy in ideal case will be similar to R^2
#Calculate MAPE - (Mean absolute % error) 
mape=sum((abs(corrected_weather$MinTemp - corrected_weather$predicted_min_temp)))/sum(corrected_weather$MinTemp)
#Calculate accuracy as 1-MAPE
accuracy=1-mape
accuracy

#Q-Q plot
ggplot(corrected_weather, aes(sample=residuals)) +
  stat_qq() +
  stat_qq_line()

#goodness of fit test
chisq <- chisq.test(corrected_weather)


#############################################################################################################

