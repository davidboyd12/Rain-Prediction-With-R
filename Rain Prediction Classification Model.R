library(Rcpp)
library(e1071)
library(caTools)
library(rpart)
library(caret)
library(ggplot2)
data = read.csv("C:\\Users\\boydd\\OneDrive\\Documents\\weatherAUS.csv")
head(data)
data$RainToday = ifelse(data$RainToday=="Yes",1,0)
head(data)
data$RainTomorrow = ifelse(data$RainTomorrow=="Yes",1,0)
table(data$RainToday)
table(data$RainTomorrow)

data$RainToday = as.integer(data$RainToday)
data$RainTomorrow = as.integer(data$RainTomorrow)
sum(is.na(data))
summary(data)

data = na.omit(data)

head(data)

#Create dummy variables for location
table(data$Location)

#Mount gambier, nuriootpa, woomera = south australia
#PERTH = WEST australia
data$Location[data$Location == "AliceSprings" | data$Location == "Darwin"] <- "NT"
data$Location[data$Location == "Brisbane" | data$Location == "Cairns" | data$Location == "Townsville"] <- "QLD"
data$Location[data$Location == "Canberra" | data$Location == "Cobar" | data$Location == "CoffsHarbour" | data$Location == "Moree" |
              data$Location == "NorfolkIsland" | data$Location == "Sydney" | data$Location == "SydneyAirport" |
              data$Location == "WaggaWagga" | data$Location == "Williamtown"] <- "NSW"
data$Location[data$Location == "Hobart"] <- "TS"
data$Location[data$Location == "Melbourne" | data$Location == "MelbourneAirport" | data$Location == "Mildura" | data$Location == "Portland" |
              data$Location == "Sale" | data$Location == "Watsonia"] <- "VT"
data$Location[data$Location == "MountGambier" | data$Location == "Nuriootpa" | data$Location == "Woomera"] <- "SA"
data$Location[data$Location == "Perth" | data$Location == "PerthAirport"] <- "WA"

table(data$Location)

#Now create dummy variables
data$NSW = ifelse(data$Location == "NSW",1,0)
data$NT = ifelse(data$Location == "NT",1,0)
data$QLD = ifelse(data$Location == "QLD",1,0)
data$SA = ifelse(data$Location == "SA",1,0)
data$TS = ifelse(data$Locatioin == "TS",1,0)
data$VT = ifelse(data$Location == "VT",1,0)
data$WA = ifelse(data$Location == "WA",1,0)

#Drop columns date, location,winddir3pm, winddir9am, windgustdir
df = subset(data, select = -c(Date, Location))
df = subset(df, select = -c(WindGustDir, WindDir9am, WindDir3pm))
#Split the data
spl = sample.split(df$RainTomorrow, SplitRatio = 0.7)
train = subset(df, spl==TRUE)
test = subset(df,spl==FALSE)

model_glm = glm(RainTomorrow ~.-Temp3pm -Humidity9am -Evaporation -Temp9am, data = train, family="binomial")
summary(model_glm)
predictTrain = predict(model_glm, train, type="response")
trainTab = table(train$RainTomorrow, predictTrain >= 0.3)
trainAcc = sum(diag(trainTab))/sum(trainTab)
trainAcc
trainTab

predictTest = predict(model_glm, test, type="response")
testTab = table(test$RainTomorrow, predictTest >= 0.33)
testTab
testAcc = sum(diag(testTab))/sum(testTab)
testAcc
#test accuracy of about 85 percent

#Can try a cart model
fit.tree = rpart(RainTomorrow ~.-Temp3pm -Humidity9am -Evaporation -Temp9am -Cloud9am -WindSpeed9am -Rainfall-WindSpeed3pm, data=train, method = 'class')
summary(fit.tree)
predict_tree = predict(fit.tree, train, type='class')
table_tree = table(train$RainTomorrow, predict_tree)
table_tree
accuracy_tree = sum(diag(table_tree))/sum(table_tree)
accuracy_tree
