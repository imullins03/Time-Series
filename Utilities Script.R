install.packages("caTools") #sample split
library("caTools")
install.packages("glmnet") #Ridge, Lasso and Elastic Net Regression
library("glmnet")
install.packages("forecast") #Time Series
library("forecast")
install.packages("tseries") #Time Series
library("tseries")
install.packages("e1071") #Support Vector Machines; Confusion Matrix
library("e1071")
install.packages("caret") #Different Analysis Tools
library("caret")
install.packages("DMwR") #Min, Max, Accuracy and Mape
library("DMwR")
install.packages("ggplot2") #Different Plots
library("ggplot2")
install.packages("reshape2") #Data Manipulation
library("reshape2")
install.packages("ggcorrplot") #Correlation Plot
library("ggcorrplot")
install.packages("xts") #Excel XTS Format Manipulation
library("xts")
install.packages("psych")
library("psych")
install.packages("data.table")
library("data.table")
install.packages("tsoutliers")
library("tsoutliers")
install.packages("boot")
library("boot")

#Upload Data and Data Wrangle
mydata <- read.csv("C:/Users/imull/Documents/Utilities2.csv")
head(mydata) #shows fist 6 entries for each variable
summary(mydata) #decriptive stats on each variable
mydata$Entry = NULL #removes from dataset,variable has no effect on analysis, just entry number
mydata$notes = NULL #variable has effect, as such dummy variable "notes1" created 
#0 being no note on account and 1 being note. Original with string data removed
colSums(is.na(mydata)) #checking for N/A in data

#Wrangling to remove any data thats not numeric in data type or needed for correlation matrix
str(mydata) #check data types for each column
mydata2 <- mydata
mydata2$notes1 = NULL
mydata2$month = NULL
mydata2$day = NULL
mydata2$year = NULL
mydata2$date = NULL
mydata2$monthsSinceY2K = NULL
as.numeric(mydata2$temp) #changes data type of variable to numeric (for correlation graph)
as.numeric(mydata2$kwh)
as.numeric(mydata2$ccf)
corrmatrix <- cor(mydata2)
corrmatrix
corrheat <- ggcorrplot(corrmatrix)
corrheat
pairs.panels(mydata2)

#Time Series and Exploration
mydata3 <- mydata[1:nrow(mydata),1:ncol(mydata)]
head(mydata3)
plot(data = mydata3, x = mydata3$month, y = mydata3$totalbill)
cycle(data = mydata3, x = mydata3$month, y = mydata3$totalbill)
boxplot(mydata3$totalbill ~ mydata3$month)
#If interested in seeing other variables to decide additive or multiplicative
#boxplot(mydata$elecbill ~ mydata$month)
#boxplot(mydata$gasbill ~ mydata$month)
#boxplot(mydata2$temp ~ mydata2$month)

#Decompose Data
#Would need code if date data not easily changed in excel
#mydata$date <- as.Date(with(mydata, paste(month, day, year,sep="-")), "%m-%d-%y")
#mydata3$date <- as.Date(mydata3$date, format = "%m/%d/%y")
data3 <- data.table(mydata3$totalbill)
tsdata <- ts(data = data3, frequency = 12)
tsoutliers(tsdata)
#tso(tsdata)
#checks outliers using ARIMA Model
ddata <- decompose(tsdata, "multiplicative")
plot(ddata)
plot(tsdata)

#Spliting into training and test sets
set.seed(112) #set data randomizer so repeat experiments can happen
trainSlices <- ts(tsdata[1:108,]) #1st nine years as training set
testSlices <- ts(tsdata[109:132,]) #last 2 years as test set

#Fit and Compare Models
#ETS Model
tr.model_ets <- ets(trainSlices)
ftr.model_ets <- forecast(tr.model_ets, h = 12*2)
model_ets <- ets(testSlices)
checkresiduals(ftr.model_ets)
checkresiduals(model_ets)
pacf(ts(model_ets$residuals), main = "PACF Residual")
accuracy(ftr.model_ets)
accuracy(model_ets)

#Holt's Model
tr.model_h <- holt(trainSlices)
ftr.model_h <- holt(trainSlices, h = 12*2)
model_h <- holt(tsdata)
checkresiduals(ftr.model_h)
checkresiduals(model_h)
pacf(ts(model_h$residuals), main = "PACF Residual")
accuracy(ftr.model_h)
accuracy(model_h)

#Holt Winters' Model: can't use training and testing set as this partition causes error
model_hw <- HoltWinters(tsdata, seasonal = "multiplicative")
checkresiduals(model_hw)
pacf(ts(model_hw$residuals), main = "PACF Residual")
accuracy(model_hw)

#ARIMA Model
tr.model_arima <- auto.arima(trainSlices)
ftr.model_arima <- forecast(tr.model_arima, h = 12*2)
model_arima <- auto.arima(tsdata)
checkresiduals(ftr.model_arima)
checkresiduals(model_arima)
pacf(ts(model_arima$residuals), main = "PACF Residual")
accuracy(ftr.model_arima)
accuracy(model_arima)

#Time Series Regression Model:can't use training and testing set as this partition causes error
model_reg <- tslm(tsdata ~ trend + season)
model_reg
checkresiduals(model_reg)
pacf(ts(model_reg$residuals), main = "PACF Residual")
accuracy(model_reg)

#Other Validation of Time Series
AIC(model_arima, model_reg)

#Plots
plot(forecast(model_ets, h = 12*2))
plot(holt(tsdata, h = 12*2))
plot(forecast(model_hw, h = 12*2))
plot(forecast(model_arima, h = 12*2))
plot(forecast(model_reg, h = 12*2))

#Add all Charts to 3 visuals out of interest
split.screen(figs = c(2,1))
screen(1)
plot(forecast(model_ets, 12*2), main = "ETS Forecast")
screen(2)
split.screen(figs = c(2,1))
plot(forecast(model_h, 12*2), main = "Holt's Forcast")
screen(1)
plot(forecast(model_hw, 12*2), main = "Holt Winters' Forecast")
split.screen(figs = c(2,1))
screen(1)
plot(forecast(model_arima, h = 12*2), main = "ARIMA Forecast")
screen(2)
plot(forecast(model_reg, h = 12*2), main = "Linear Regression Forecast")
dev.off()

#Regression Analysis
#General Splitting of data into trainng and test sets
set.seed(110)
attach(mydata)
totalbill <- as.integer(mydata$totalbill)
mydata4 <- data.table(totalbill, temp, kwh, ccf, therms)
y <- as.matrix(totalbill)
x <- as.matrix(data.table(temp, kwh, ccf, therms))
trainingindex <- sample(1:nrow(mydata4), 0.7*nrow(mydata4))
training <- mydata4[trainingindex,]
test <- mydata4[-trainingindex,]
x.train <- x[trainingindex,]
x.test <- x[-trainingindex,]
y.train <- y[trainingindex]
y.test <- y[-trainingindex]
#trainigindex <- sample.split(mydata, splitRatio = 0.7)
#training <- subset(mydata, splitl == "true")
#test <- subset(mydata, splitl == "false")

#GLM Regression
regmodel1 <- glm( totalbill~ .,  
                 family = "gaussian", data = training)
regmodelt <- glm(totalbill ~ .,  
                 family = "gaussian", data = test)
premodel1 <- predict(regmodelt, test, type = "response")
premodel1
summary(premodel1)
mse <- mean((test$totalbill - premodel1)^2)
mse
regmodel <- glm(totalbill ~., family = "gaussian", data = mydata4)
accuracy(test$totalbill, premodel1)

#Cross Validation Linear Regression 
cvmodel10 <- cv.glm(data = training, glmfit = regmodel, K = 10)
cv.err10 <- cvmodel10$delta
cv.err10
cvmodel5 <- cv.glm(data = training, glmfit = regmodel, K = 5)
cv.err5 <- cvmodel5$delta
cv.err5
cvmodel1OneOut <- cv.glm(data = training, glmfit = regmodel)
cv.errl1o <- cvmodel1OneOut$delta
cv.errl1o

#Ridge, Lasso and Elastic Net Regression with 10 Fold Cross Validation
list.of.fit <- list()
for (i in 0:10) {
  fit.name <- paste0("Alpha", i/10)
  list.of.fit[[fit.name]]<-
  cv.glmnet(x.train, y.train,type.measure = "mse", alpha = i/10, family = "gaussian")
}

results <- data.frame()

for (i in 0:10){
  fit.name <- paste0("Alpha", i/10)
  predited <- predict(list.of.fit[[fit.name]], 
                      s = list.of.fit[[fit.name]]$lambda.1se, newx = x.test)
  mse <- mean((y.test - predited)^2)
  temps <- data.frame(fit.name = fit.name, alpha = i/10, mse = mse)
  results <- rbind(results, temps)
}

results
tcvenmodel <- cv.glmnet(x.train, y.train,type.measure = "mse", alpha = 7/10, family = "gaussian")
summary(tcvenmodel)
ptcvemodel <- predict(tcvenmodel, newx = x.test, type = "response")
ptcmse <- mean((y.test - ptcvemodel)^2)
ptcmse
accuracy(y.test, ptcvemodel)

#SVM Regression
svm.model <- svm(x.train, y.train, type = "eps-regression", kernel = "linear", cross = 10)
svm.model5 <- svm(x.train, y.train, type = "eps-regression", kernel = "linear", cross = 5)
svm.pre <- predict(svm.model, newdata = x.test, type = "response")
svm.pre5 <- predict(svm.model5, newdata = x.test, type = "response")
svm.mse <- mean((y.test - svm.pre)^2)
svm.mse5 <- mean((y.test - svm.pre5)^2)
svm.mse
svm.mse5
accuracy(svm.pre, y.test)
svm.model3 <- svm(x, y, type = "eps-regression", kernel = "linear", cross = 10)
svm.pre3 <- svm.predict(svm.model3)
plot(svm.pre, h = 12*2)
