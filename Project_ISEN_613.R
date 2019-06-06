#Clears the workspace
remove(list=ls())

#List of Custom file built
# Differencing(data-set, order of differencing to be done(like 1,2,3...))
# Dataform(data-set, "transformation function") [Transformation Functions available - Log, sqrt, raw (means original data), percent]

# sourcing all the custom files built into R-environment
#Command : source("filepath with the file name")
source("C:/Laxmi/ISEN 613/Project/Project_codes/Dataform.R")
source("C:/Laxmi/ISEN 613/Project/Project_codes/Differencing.R") 

'
#Packages to be installed
install.packages("tseries")
install.packages("zoo")
install.packages("leaps")
install.packages("locfit")
install.packages("mgcv")
install.packages("nlme")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("forecast")
install.packages("rugarch")
install.packages("e1071")
install.packages("fGarch")
install.packages("FinTS", repos = "http://R-Forge.R-project.org")
install.packages("funitRoots", repos = "http://R-Forge.R-project.org")
'


#List of Libraries
library(openxlsx)
library(tseries)
library(zoo)
library(gridExtra)
library(leaps)
library(locfit)
library(nlme)
library(mgcv)
library(readxl)
library(dplyr)
library(forecast)
library(FinTS)
library(e1071)
library(funitRoots)
library(rugarch)
library(fGarch)

#reading data file. Enclosed in " " i.e "C:/Laxmi/ISEN 613/Project/Project_Data_Set.xlsx" is file path
input <- read.csv(file = "C:/Laxmi/ISEN 613/Project/Project_Data_Set_S_and_P_500.csv" , header=TRUE, sep = ",")

#"Stocks" in the data-set are uniquely identified and stored as row vector in "stocks_list"
stocks_list = unique(input$Name)

# "while" loop has been used to evaluate and model data for each stock. 
# subroutine files, like "Differences" has been called in loop, so as to be evaluated 
#each time the loop is run.
i = 1

input_data = as.data.frame(input[input$Name == stocks_list[i],])
#Transforming data to be used for model development and testing
#*************************************
model_data = Dataform(input_data,"raw")
index_column = matrix(c(seq(from = 0, to = ((nrow(model_data))-1), by = 1)),ncol=1)
model_data = data.frame(model_data, index_column)
colnames(model_data)[(ncol(model_data))] <- "index"
  
  
#Plots of data - Automatic Generation
j=1
while(j <= (ncol(model_data)-1))
  {
    lm.fit1 = lm(model_data[,j]~index, data = model_data)
    'lm.fit2 = lm(model_data[,j]~poly(index,2), data = model_data)
    lm.fit3 = lm(model_data[,j]~poly(index,3), data = model_data)
    
    best_fit = which.max(c(summary(lm.fit1)$adj.r.squared,summary(lm.fit2)$adj.r.squared,summary(lm.fit3)$adj.r.squared))
    '
    #if (best_fit == 1)
    {lm.fit = lm.fit1}
    'f (best_fit == 2)
    {lm.fit = lm.fit2}
    if (best_fit == 3)
    {lm.fit = lm.fit3}' 
      
  file_name = paste("Plot of", names(model_data)[j], " of ", stocks_list[i])
  jpeg(paste(file_name,".jpeg"))
  par(mfrow=c(2,1))
  plot(model_data[,j], xlab = "Index", ylab = names(model_data)[j] , main = file_name )
  lines(predict(lm.fit), lwd=3, col="blue")
  plot((model_data[,j]-predict(lm.fit, data=model_data)), xlab = "Index", ylab = paste("Residuals of", names(model_data)[j]), main = paste(file_name, "residuals"))
  dev.off()
  j = j+1
}

#Stationarizing the data by differencing and Plotting the same
#*************************************************************
order_of_differencing = 1
model_data_diff = Differencing(model_data,order_of_differencing)
j=1
while(j <= ncol(model_data_diff))
{
  file_name = paste("Plot of", names(model_data)[j], "of", stocks_list[i], " stock differenced ", order_of_differencing , "time in order")
  jpeg(paste(file_name,".jpeg"))
  plot(model_data_diff[,j], xlab = "Index", ylab = paste("Difference of",names(model_data)[j]) , main = file_name , type="l")
  dev.off()
  j = j+1
}

#*************************************************************
analysis_data = model_data_diff #this is the data that is stationary and further worked upon in 
                                # in ARIMA modelling and GARCH Modelling   

#Dickey-Fuller Test on individual column of "analysis_data". We can feed any data
j=1
adf.test_p_value = matrix(c(rep(0,(ncol(analysis_data)-1)*4)),ncol=4)
while(j <= (ncol(analysis_data)-1))
{
  a = adf.test(analysis_data[,j], alternative ="stationary", k=0)
  #"k" is the number of "lag" involved. If "k" is not specified, we will "k" for which p-value is least
  #Null Hypothesis: Not Stationary || Alternative Hypothesis = Stationary
  # p > 0.05 : Null Hypothesis is "True", otherwise, alternate is true.
  adf.test_p_value[j,2] = a$p.value #Storing p-value
  adf.test_p_value[j,1] = paste(stocks_list[i],"_",names(analysis_data)[j]) #Storing the name of data_set to which the p-value belongs 
  
  a = adf.test(analysis_data[,j], alternative ="stationary")
  adf.test_p_value[j,3] = a$parameter
  adf.test_p_value[j,4] = a$p.value
  write.table(adf.test_p_value, paste("C:/Laxmi/ISEN 613/Project/a1_log_diff.txt"), sep="\t")
  j = j+1
}

#ACF and PACF plots are generated on stationary data. Parameters 
#p - estimated from PACF plots
#q - estimated from ACF plots

j = 1
while(j <= (ncol(analysis_data)-1))
{
  jpeg(paste("ACF plot of", names(analysis_data)[j],"of", stocks_list[i],".jpeg"))
  plot(acf(analysis_data[,j]), main=paste("ACF plot of",names(analysis_data)[j],"of",stocks_list[i]))
  dev.off()
  
  jpeg(paste("PACF plot of", names(analysis_data)[j],"of", stocks_list[i],".jpeg"))
  plot(pacf(analysis_data[,j]), main=paste("PACF plot of",names(analysis_data)[j],"of",stocks_list[i]))
  dev.off()
  j=j+1
}

#########################################################
#########################################################
#                   ARIMA MODELIING                     #
#########################################################
#########################################################
  
#Splitting data into "training" and "testing"
#For ARIMA original data, i.e raw data. Any differencing done for stationarity
#is fed in parameters (p,q,d), as it is.
train = seq(1:(floor(0.7*nrow(model_data))))
test = seq(from=length(train)+1, to=(nrow(model_data)), by=1)
number_of_future = ((nrow(model_data))-(length(train))-1)
  
#This section of code is the individually evaluate various combinations of ARIMA(p,d,q)
# parameters (p,d,q)
  
j=4
#while(j <= (ncol(model_data)-1))
#{
  arima.model1 = arima(model_data[train,j], order=c(1,0,0))
  pred.model1 = forecast(arima.model1,h=number_of_future)
  file_name_ar_1 = paste("Plot of ARIMA(1,0,0) of",names(model_data)[j]," of ",stocks_list[i])
  max_upper_1 = (pred.model1$upper[which.max(pred.model1$upper)])+100
  min_lower_1 = (pred.model1$lower[which.min(pred.model1$lower)])-100
  max_residual_1 = pred.model1$residuals[which.max(pred.model1$residuals)]
  min_residual_1 = pred.model1$residuals[which.min(pred.model1$residuals)]
  jpeg(paste(file_name_ar_1,".jpeg"))
  par(mfrow = c(2,1))
  plot.ts(model_data[(length(train)+1):nrow(model_data),6], model_data[(length(train)+1):nrow(model_data),j],xlab="Index", ylab=names(model_data)[j],main=paste(file_name_ar_1,names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_lower_1, max_upper_1))
  lines(pred.model1$mean, lwd = 3, col="red")
  lines(pred.model1$upper[,1],lwd = 1, col="blue")
  lines(pred.model1$lower[,1],lwd = 1, col="blue")
  lines(pred.model1$upper[,2],lwd = 1, col="green")
  lines(pred.model1$lower[,2],lwd = 1, col="green")
  plot(pred.model1$residuals, xlab="Index", ylab="Residuals of Forecasted Values", main=paste("Residuals of Forecasted Values",names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_residual_1, max_residual_1))
  dev.off()
  
  arima.model2 = arima(model_data[train,j], order=c(0,1,0))
  pred.model2 = forecast(arima.model2,h=number_of_future)
  file_name_ar_2 = paste("Plot of ARIMA(0,1,0) of",names(model_data)[j]," of ",stocks_list[i])
  max_upper_2 = (pred.model2$upper[which.max(pred.model2$upper)])+100
  min_lower_2 = (pred.model2$lower[which.min(pred.model2$lower)])-100
  max_residual_2 = pred.model2$residuals[which.max(pred.model2$residuals)]
  min_residual_2 = pred.model2$residuals[which.min(pred.model2$residuals)]
  jpeg(paste(file_name_ar_2,".jpeg"))
  par(mfrow = c(2,1))
  plot.ts(model_data[(length(train)+1):nrow(model_data),6], model_data[(length(train)+1):nrow(model_data),j],xlab="Index", ylab=names(model_data)[j],main=paste(file_name_ar_2,names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_lower_2, max_upper_2))
  lines(pred.model2$mean, lwd = 3, col="red")
  lines(pred.model2$upper[,1],lwd = 1, col="blue")
  lines(pred.model2$lower[,1],lwd = 1, col="blue")
  lines(pred.model2$upper[,2],lwd = 1, col="green")
  lines(pred.model2$lower[,2],lwd = 1, col="green")
  plot(pred.model2$residuals, xlab="Index", ylab="Residuals of Forecasted Values", main=paste("Residuals of Forecasted Values",names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_residual_2, max_residual_2))
  dev.off()
  
  arima.model3 = arima(model_data[train,j], order=c(0,0,1))
  pred.model3 = forecast(arima.model3,h=number_of_future)
  file_name_ar_3 = paste("Plot of ARIMA(0,0,1) of",names(model_data)[j]," of ",stocks_list[i])
  max_upper_3 = (pred.model3$upper[which.max(pred.model3$upper)])+100
  min_lower_3 = (pred.model3$lower[which.min(pred.model3$lower)])-100
  max_residual_3 = pred.model3$residuals[which.max(pred.model3$residuals)]
  min_residual_3 = pred.model3$residuals[which.min(pred.model3$residuals)]
  jpeg(paste(file_name_ar_3,".jpeg"))
  par(mfrow = c(2,1))
  plot.ts(model_data[(length(train)+1):nrow(model_data),6], model_data[(length(train)+1):nrow(model_data),j],xlab="Index", ylab=names(model_data)[j],main=paste(file_name_ar_3,names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_lower_3, max_upper_3))
  lines(pred.model3$mean, lwd = 3, col="red")
  lines(pred.model3$upper[,1],lwd = 1, col="blue")
  lines(pred.model3$lower[,1],lwd = 1, col="blue")
  lines(pred.model3$upper[,2],lwd = 1, col="green")
  lines(pred.model3$lower[,2],lwd = 1, col="green")
  plot(pred.model3$residuals, xlab="Index", ylab="Residuals of Forecasted Values", main=paste("Residuals of Forecasted Values",names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_residual_3, max_residual_3))
  dev.off()
  
  arima.model4 = arima(model_data[train,j], order=c(0,1,1))
  pred.model4 = forecast(arima.model4,h=number_of_future)
  file_name_ar_4 = paste("Plot of ARIMA(0,1,1) of",names(model_data)[j]," of ",stocks_list[i])
  max_upper_4 = (pred.model4$upper[which.max(pred.model4$upper)])+100
  min_lower_4 = (pred.model4$lower[which.min(pred.model4$lower)])-100
  max_residual_4 = pred.model4$residuals[which.max(pred.model4$residuals)]
  min_residual_4 = pred.model4$residuals[which.min(pred.model4$residuals)]
  jpeg(paste(file_name_ar_4,".jpeg"))
  par(mfrow = c(2,1))
  plot.ts(model_data[(length(train)+1):nrow(model_data),6], model_data[(length(train)+1):nrow(model_data),j],xlab="Index", ylab=names(model_data)[j],main=paste(file_name_ar_4,names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_lower_4, max_upper_4))
  lines(pred.model4$mean, lwd = 3, col="red")
  lines(pred.model4$upper[,1],lwd = 1, col="blue")
  lines(pred.model4$lower[,1],lwd = 1, col="blue")
  lines(pred.model4$upper[,2],lwd = 1, col="green")
  lines(pred.model4$lower[,2],lwd = 1, col="green")
  plot(pred.model4$residuals, xlab="Index", ylab="Residuals of Forecasted Values", main=paste("Residuals of Forecasted Values",names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_residual_4, max_residual_4))
  dev.off()
  
  arima.model5 = arima(model_data[train,j], order=c(1,1,0))
  pred.model5 = forecast(arima.model5,h=number_of_future)
  file_name_ar_5 = paste("Plot of ARIMA(1,1,0) of",names(model_data)[j]," of ",stocks_list[i])
  jpeg(paste(file_name_ar_5,".jpeg"))
  max_upper_5 = (pred.model5$upper[which.max(pred.model5$upper)])+100
  min_lower_5 = (pred.model5$lower[which.min(pred.model5$lower)])-100
  max_residual_5 = pred.model5$residuals[which.max(pred.model5$residuals)]
  min_residual_5 = pred.model5$residuals[which.min(pred.model5$residuals)]
  par(mfrow = c(2,1))
  plot.ts(model_data[(length(train)+1):nrow(model_data),6], model_data[(length(train)+1):nrow(model_data),j],xlab="Index", ylab=names(model_data)[j],main=paste(file_name_ar_5,names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_lower_5, max_upper_5))
  lines(pred.model5$mean, lwd = 3, col="red")
  lines(pred.model5$upper[,1],lwd = 1, col="blue")
  lines(pred.model5$lower[,1],lwd = 1, col="blue")
  lines(pred.model5$upper[,2],lwd = 1, col="green")
  lines(pred.model5$lower[,2],lwd = 1, col="green")
  plot(pred.model5$residuals, xlab="Index", ylab="Residuals of Forecasted Values", main=paste("Residuals of Forecasted Values",names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_residual_5, max_residual_5))
  dev.off()
  
  arima.model6 = arima(model_data[train,j], order=c(1,0,1))
  pred.model6 = forecast(arima.model6,h=number_of_future)
  file_name_ar_6 = paste("Plot of ARIMA(1,0,1) of",names(model_data)[j]," of ",stocks_list[i])
  max_upper_6 = (pred.model6$upper[which.max(pred.model6$upper)])+100
  min_lower_6 = (pred.model6$lower[which.min(pred.model6$lower)])-100
  max_residual_6 = pred.model6$residuals[which.max(pred.model6$residuals)]
  min_residual_6 = pred.model6$residuals[which.min(pred.model6$residuals)]
  jpeg(paste(file_name_ar_6,".jpeg"))
  par(mfrow = c(2,1))
  plot.ts(model_data[(length(train)+1):nrow(model_data),6], model_data[(length(train)+1):nrow(model_data),j],xlab="Index", ylab=names(model_data)[j],main=paste(file_name_ar_6,names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_lower_6, max_upper_6))
  lines(pred.model6$mean, lwd = 3, col="red")
  lines(pred.model6$upper[,1],lwd = 1, col="blue")
  lines(pred.model6$lower[,1],lwd = 1, col="blue")
  lines(pred.model6$upper[,2],lwd = 1, col="green")
  lines(pred.model6$lower[,2],lwd = 1, col="green")
  plot(pred.model6$residuals, xlab="Index", ylab="Residuals of Forecasted Values", main=paste("Residuals of Forecasted Values",names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_residual_6, max_residual_6))
  dev.off()
 
  summary(arima.model1)
  summary(arima.model2)
  summary(arima.model3)
  summary(arima.model4)
  summary(arima.model5)
  summary(arima.model6)
  
  #Diagnostic Plots for above
  jpeg(paste("Diagnostic Plots of ARIMA(1,0,0)",names(model_data)[j],"of",stocks_list[i],".jpeg"))
  par(mfrow=c(2,2))
  qqnorm(arima.mode$residuals, main = paste("qqplot of residuals of ARIMA(1,0,0)",names(model_data)[j],"of",stocks_list[i]) )
  plot(acf(arima.mode1$residuals), main=paste("ACF plot of residuals of ARIMA(1,0,0)",names(model_data)[j],"of",stocks_list[i]))
  hist((arima.model1$residuals))
  dev.off()
  
  jpeg(paste("Diagnostic Plots of ARIMA(0,1,0)",names(model_data)[j],"of",stocks_list[i],".jpeg"))
  par(mfrow=c(2,2))
  qqnorm(arima.model2$residuals, main = paste("qqplot of residuals of ARIMA(0,1,0)",names(model_data)[j],"of",stocks_list[i]) )
  plot(acf(arima.model2$residuals), main=paste("ACF plot of residuals of ARIMA(0,1,0)",names(model_data)[j],"of",stocks_list[i]))
  hist((arima.model2$residuals))
  dev.off()
  
  jpeg(paste("Diagnostic Plots of ARIMA(0,0,1)",names(model_data)[j],"of",stocks_list[i],".jpeg"))
  par(mfrow=c(2,2))
  qqnorm(arima.model3$residuals, main = paste("qqplot of residuals of ARIMA(0,0,1)",names(model_data)[j],"of",stocks_list[i]) )
  plot(acf(arima.model3$residuals), main=paste("ACF plot of residuals of ARIMA(0,0,1)",names(model_data)[j],"of",stocks_list[i]))
  hist((arima.model3$residuals))
  dev.off()
  
  jpeg(paste("Diagnostic Plots of ARIMA(0,1,1)",names(model_data)[j],"of",stocks_list[i],".jpeg"))
  par(mfrow=c(2,2))
  qqnorm(arima.model4$residuals, main = paste("qqplot of residuals of ARIMA(0,1,1)",names(model_data)[j],"of",stocks_list[i]) )
  plot(acf(arima.model4$residuals), main=paste("ACF plot of residuals of ARIMA(0,1,1)",names(model_data)[j],"of",stocks_list[i]))
  hist((arima.model4$residuals))
  dev.off()
  
  jpeg(paste("Diagnostic Plots of ARIMA(1,1,0)",names(model_data)[j],"of",stocks_list[i],".jpeg"))
  par(mfrow=c(2,2))
  qqnorm(arima.model5$residuals, main = paste("qqplot of residuals of ARIMA(1,1,0)",names(model_data)[j],"of",stocks_list[i]) )
  plot(acf(arima.model5$residuals), main=paste("ACF plot of residuals of of ARIMA(1,1,0)",names(model_data)[j],"of",stocks_list[i]))
  hist((arima.model5$residuals))
  dev.off()
  
  jpeg(paste("Diagnostic Plots of ARIMA(1,0,1)",names(model_data)[j],"of",stocks_list[i],".jpeg"))
  par(mfrow=c(2,2))
  qqnorm(arima.model6$residuals, main = paste("qqplot of residuals ARIMA(1,0,1) of",names(model_data)[j],"of",stocks_list[i]) )
  plot(acf(arima.model6$residuals), main=paste("ACF plot of residualsARIMA(1,0,1)",names(model_data)[j],"of",stocks_list[i]))
  hist((arima.model6$residuals))
  dev.off()
#}

#Development of Arima Model. The following command returns the best ARIMA parameters
j=4

#while(j <= (ncol(model_data)-1))
#{
  arima.model_1 = auto.arima(model_data[train,j], max.p = 10, max.q = 10, max.d = 10)
  #predicting / forecasting "n.ahead" and "h" values into future
  forecast.model1=forecast(arima.model_1,h=number_of_future)
  file_name_ar_best = paste("Plot of best ARIMA output using 'auto.arima' of",names(model_data)[j]," of ",stocks_list[i])
  max_upper = (forecast.model1$upper[which.max(forecast.model1$upper)])+100
  min_lower = (forecast.model1$lower[which.min(forecast.model1$lower)])-100
  
  max_residual = forecast.model1$residuals[which.max(forecast.model1$residuals)]
  min_residual = forecast.model1$residuals[which.min(forecast.model1$residuals)]
  jpeg(paste(file_name_ar_best,".jpeg"))
  par(mfrow=c(2,1))
  plot.ts(model_data[(length(train)+1):nrow(model_data),6], model_data[(length(train)+1):nrow(model_data),j],xlab="Index", ylab=names(model_data)[j],lwd=1, col="black", type="l", ylim = c(min_lower, max_upper))
  lines(forecast.model1$mean, lwd = 3, col="red")
  lines(forecast.model1$upper[,1],lwd = 1, col="blue")
  lines(forecast.model1$lower[,1],lwd = 1, col="blue")
  lines(forecast.model1$upper[,2],lwd = 1, col="green")
  lines(forecast.model1$lower[,2],lwd = 1, col="green")
  plot(forecast.model1$residuals, xlab="Index", ylab="Residuals of Forecasted Values", main=paste("Residuals of Forecasted Values",names(model_data)[j]),lwd=1, col="black", type="l", ylim = c(min_residual, max_residual))
  dev.off()
  
  jpeg(paste("Diagnostic Plots of",names(model_data)[j],"of",stocks_list[i],".jpeg"))
  par(mfrow=c(2,2))
  qqnorm(forecast.model1$residuals, main = paste("qqplot of residuals",names(model_data)[j],"of",stocks_list[i]) )
  plot(acf(forecast.model1$residuals), main=paste("ACF plot of residuals",names(model_data)[j],"of",stocks_list[i]))
  hist((forecast.model1$residuals))
  dev.off()
  
  summary(arima.model_1)
  Box.test(forecast.model1$residuals^2,lag=20,type = "Ljung-Box")
  
  j=j+1
}

#########################################################
#########################################################
#                         GARCH                         #
#########################################################
#########################################################

#PRE-TESTING
#Statistics about the data assigned to "FinTS.stats_anly_data"
j = 1
#************************************************************
FinTS.stats_anly_data = model_data #"Data to be analysed

FinTS.stats_data = matrix(c(rep(0, ((ncol(FinTS.stats_anly_data)-1)*9))), ncol=9) #8 attributes in "FinTS.stats" command 
colnames(FinTS.stats_data) = c("Name of Quantity","Start", "Size", "Mean", "Standard.Deviation", "Skewness","Excess.Kurtosis", "Minimum","Maximum")
while(j <= (ncol(FinTS.stats_anly_data)-1))
{
  stats_fin = FinTS.stats(FinTS.stats_anly_data[,j])
  FinTS.stats_data[j,1] = paste(stocks_list[i],"_",names(FinTS.stats_anly_data)[j])
  'k = 2
  while(k <= (ncol(FinTS.stats_data)))
  {
    FinTS.stats_data[,k] = stats_fin$names(FinTS.stats_data)[k]
    k=k+1
  }'
  FinTS.stats_data[j,2] = stats_fin$Start
  FinTS.stats_data[j,3] = stats_fin$Size
  FinTS.stats_data[j,4] = stats_fin$Mean
  FinTS.stats_data[j,5] = stats_fin$Standard.Deviation
  FinTS.stats_data[j,6] = stats_fin$Skewness
  FinTS.stats_data[j,7] = stats_fin$Excess.Kurtosis
  FinTS.stats_data[j,8] = stats_fin$Minimum
  FinTS.stats_data[j,9] = stats_fin$Maximum
  j=j+1
}

#"Jb.test" of "FinTS.stats_anly_data" stored in "FinTS.stats_JBtest_data"
j=1
FinTS.stats_JBtest_data = matrix(c(rep(0, ((ncol(FinTS.stats_anly_data)-1)*4))), ncol=4) #3 attributes in "FinTS.stats" command 
colnames(FinTS.stats_JBtest_data) = c("Name of Quantity","X-squared", "df", "p-value")
while(j <= (ncol(FinTS.stats_anly_data)-1))
{
  jb_test = jarque.bera.test(FinTS.stats_anly_data[,j])
  FinTS.stats_JBtest_data[j,1] = paste("JB test on",stocks_list[i],"_",names(FinTS.stats_anly_data)[j])
  FinTS.stats_JBtest_data[j,2] = jb_test$statistic
  FinTS.stats_JBtest_data[j,3] = jb_test$parameter
  FinTS.stats_JBtest_data[j,4] = jb_test$p.value
  j=j+1
}

#ARCH test on "FinTS.stats_anly_data"
j=1
#*********************************************************
number_of_lag = 1
FinTS.stats_Archtest_data = matrix(c(rep(0, ((ncol(FinTS.stats_anly_data)-1)*4))), ncol=4) #3 attributes in "FinTS.stats" command 
colnames(FinTS.stats_Archtest_data) = c("Name of Quantity","X-squared", "df", "p-value")
while(j <= (ncol(FinTS.stats_anly_data)-1))
{
  arch_test = ArchTest(FinTS.stats_anly_data[,j], lag = number_of_lag)
  FinTS.stats_Archtest_data[j,1] = paste("ARCH Test on",stocks_list[i],"_",names(FinTS.stats_anly_data)[j],"with lag",number_of_lag )
  FinTS.stats_Archtest_data[j,2] = arch_test$statistic
  FinTS.stats_Archtest_data[j,3] = arch_test$parameter
  FinTS.stats_Archtest_data[j,4] = arch_test$p.value
  j=j+1
}


##fitting garch model
a1=garchFit(formula = ~ garch(2,2),data = New_data$Close)
summary(a1)

a2=garchFit(formula = ~garch(1,1),data = New_data$Open)
summary(a2)

a3=garchFit(formula = ~garch(1,1),data = New_data$High)
summary(a3)

a4=garchFit(formula = ~garch(1,1),data = New_data$Low)
summary(a4)

a5=garchFit(formula = ~garch(1,1),data = New_data$Volume)
summary(a5)

predict(a1)

plot(a1)


#########################################################
#########################################################
#                         GARCH                         #
#########################################################
#########################################################

j = 1
fit1 = auto.arima(FinTS.stats_anly_data[,j],trace = TRUE, test = "kpss", ic="aic" )
j = j+1
fit2 = auto.arima(FinTS.stats_anly_data[,j],trace = TRUE, test = "kpss", ic="aic" )
j = j+1
fit3 = auto.arima(FinTS.stats_anly_data[,j],trace = TRUE, test = "kpss", ic="aic" )
j = j+1
fit4 = auto.arima(FinTS.stats_anly_data[,j],trace = TRUE, test = "kpss", ic="aic" )
j = j+1

#ARCh effect test
Box.test(fit1$residuals^2,lag=12,type = "Ljung-Box")
Box.test(fit2$residuals^2,lag=12,type = "Ljung-Box")
Box.test(fit3$residuals^2,lag=12,type = "Ljung-Box")
Box.test(fit4$residuals^2,lag=12,type = "Ljung-Box")
## less than 0.05 , therefore we reject the null hypothesis of no ARCH effect 

j=1
res.garch1_spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder= c(1,1)))
res.garch1_fit = ugarchfit(spec = res.garch1_spec, data = FinTS.stats_anly_data[,j])
print(res.garch1_fit)

j = j+1
res.garch2_spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder= c(1,1)))
res.garch2_fit = ugarchfit(spec = res.garch2_spec, data = FinTS.stats_anly_data[,j])
print(res.garch1_fit)

j = j+1
res.garch3_spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder= c(1,1)))
res.garch1_fit = ugarchfit(spec = res.garch3_spec, data = FinTS.stats_anly_data[,j])
print(res.garch3_fit)

j = j+1
res.garch4_spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder= c(1,1)))
res.garch1_fit = ugarchfit(spec = res.garch4_spec, data = FinTS.stats_anly_data[,j])
print(res.garch4_fit)


ctrl= list(tol= 1e-7, delta = 1e-9)

j = 1
res_garch1_roll = ugarchroll(res.garch1_spec,FinTS.stats_anly_data[,j],n.start = 1000,refit.every = 1,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = 0.01,keep.coef = TRUE,solver.control = ctrl,fit.control = list(scale=1))
report(res_garch1_roll, type = "Var", VaR.alpha = 0.01, conf.level=0.99)
plot(res.garch1_fit)

j+1
res_garch2_roll = ugarchroll(res.garch2_spec,FinTS.stats_anly_data[,j],n.start = 1000,refit.every = 1,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = 0.01,keep.coef = TRUE,solver.control = ctrl,fit.control = list(scale=1))
report(res_garch2_roll, type = "Var", VaR.alpha = 0.01, conf.level=0.99)
plot(res.garch2_fit)

j+1
res_garch3_roll = ugarchroll(res.garch3_spec,FinTS.stats_anly_data[,j],n.start = 1000,refit.every = 1,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = 0.01,keep.coef = TRUE,solver.control = ctrl,fit.control = list(scale=1))
report(res_garch3_roll, type = "Var", VaR.alpha = 0.01, conf.level=0.99)
plot(res.garch3_fit)

j+1
res_garch4_roll = ugarchroll(res.garch4_spec,FinTS.stats_anly_data[,j],n.start = 1000,refit.every = 1,refit.window = "moving",solver = "hybrid",calculate.VaR = TRUE,VaR.alpha = 0.01,keep.coef = TRUE,solver.control = ctrl,fit.control = list(scale=1))
report(res_garch4_roll, type = "Var", VaR.alpha = 0.01, conf.level=0.99)
plot(res.garch4_fit)

res_garch1_fcst = ugarchforecast(res.garch1_fit,n.ahead = 12)
res_garch1_fcst

res_garch2_fcst = ugarchforecast(res.garch2_fit,n.ahead = 12)
res_garch2_fcst

res_garch3_fcst = ugarchforecast(res.garch3_fit,n.ahead = 12)
res_garch3_fcst

res_garch4_fcst = ugarchforecast(res.garch4_fit,n.ahead = 12)
res_garch4_fcst


##############################################################
##############################################################

