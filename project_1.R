library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(tidyquant)
library(broom)
library(timekit)
library(modelr)
library(timetk)

data = read.csv("D:/SEMESTR V/ANALIZA-DANYCH/PredictionModels/owid-covid-data.csv")
dataGB <- data[data$iso_code == "GBR", ]

firstNA <- min(which(dataGB$new_deaths!=0))
dataGB <- dataGB[firstNA:nrow(dataGB), ]

train = dataGB[dataGB$date <= Sys.Date()-30,]  # czy do konca listopada?
test = dataGB[dataGB$date > Sys.Date()-30,]    # czy caly grudzien?

dataset <- train[, c("new_deaths", "date")]
datasetTest <- test[, c("total_deaths", "date")]

str(data)
dataGB$date = as.Date(dataGB$date)
str(data)

# wykres nowych zgonow w okresie marzec - grudzien
ggplot(dataGB, 
       aes(x=date, 
           y=new_deaths)) +
  geom_line()+
  labs(x='date', y='new deaths')

# wykres calkowitej liczby zgonow w okresie marzec - grudzien
ggplot(dataGB, 
       aes(x=date, 
           y=total_deaths)) +
  geom_line()+
  labs(x='date', y='total deaths')

# konwertowanie dat

subsetGB = dataGB %>%
  select(date, new_deaths, total_deaths)

subsetGB$day = order(a=subsetGB$date)

subsetGB$weekDay = as.numeric(format(subsetGB$date, "%u"))

subsetGB$isWeekend = ifelse(subsetGB$weekDay > 5, TRUE, FALSE)
head(subsetGB$isWeekend)

subsetGB$quarter = cut(subsetGB$date, breaks = "quarter")
head(subsetGB$quarter)

subsetGB$trainOrTest = ifelse(subsetGB$date < max(subsetGB$date)-30, "train", "test")

trainSub = subsetGB %>%
  filter(trainOrTest == "train")
testSub = subsetGB %>%
  filter(trainOrTest == "test")


# modele glm dla zbioru treningowego - ROZKLAD GAUSSA
#day
gaussianModelTrain = glm(new_deaths ~ day, data = trainSub, family = "gaussian")
summary(gaussianModelTrain)

# model regresji -> y = -0.48x + 301.02

# model_visual
# tidy(gaussianModelTrain) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)

ggplot(trainSub, aes(y=new_deaths,x=day))+geom_point()+geom_smooth(method="lm")

#quarter
gaussianModelTrain2 = glm(new_deaths ~ quarter, data = trainSub, family = "gaussian")
summary(gaussianModelTrain2)

ggplot(trainSub, aes(y=new_deaths,x=quarter))+geom_point()+geom_smooth(method="lm")

#isWeekend - czy jest sens???
# gaussianModelTrain3 = glm(new_deaths ~ isWeekend, data = trainSub, family = "gaussian")
# summary(gaussianModelTrain3)

#wieloczynnikowa
gaussianModelTrain4 = glm(new_deaths ~ day + weekDay + quarter, data = trainSub, family = "gaussian")
summary(gaussianModelTrain4)

# tidy(gaussianModelTrain2) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)



# modele glm dla zbioru testowego - ROZKLAD GAUSSA
#day
gaussianModelTest = glm(new_deaths ~ day, data = testSub, family = "gaussian")
summary(gaussianModelTest)

# model regresji -> y = 

# model_visual
# tidy(gaussianModelTest) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)

ggplot(testSub, aes(y=new_deaths,x=day))+geom_point()+geom_smooth(method="lm")

#quarter
gaussianModelTest2 = glm(new_deaths ~ quarter, data = testSub, family = "gaussian")
summary(gaussianModelTest2)

ggplot(testSub, aes(y=new_deaths,x=quarter))+geom_point()+geom_smooth(method="lm")

#isWeekend - czy jest sens???
# gaussianModelTest3 = glm(new_deaths ~ isWeekend, data = testSub, family = "gaussian")
# summary(gaussianModelTest3)

#wieloczynnikowa
gaussianModelTest4 = glm(new_deaths ~ day + weekDay + quarter, data = testSub, family = "gaussian")
summary(gaussianModelTest4)

# tidy(gaussianModelTest4) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)


# modele glm dla zbioru treningowego - ROZKLAD POISSONA
#day
poissonModelTrain = glm(new_deaths ~ day, data = trainSub, family = "poisson")
summary(poissonModelTrain)

# model regresji -> y = -0.48x + 301.02

# model_visual
# tidy(poissonModelTrain) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)

ggplot(trainSub, aes(y=new_deaths,x=day))+geom_point()+geom_smooth(method="lm")

#quarter
poissonModelTrain2 = glm(new_deaths ~ quarter, data = trainSub, family = "poisson")
summary(poissonModelTrain2)

ggplot(trainSub, aes(y=new_deaths,x=quarter))+geom_point()+geom_smooth(method="lm")

#isWeekend - czy jest sens???
# poissonModelTrain3 = glm(new_deaths ~ isWeekend, data = trainSub, family = "poisson")
# summary(poissonModelTrain3)

#wieloczynnikowa
poissonModelTrain4 = glm(new_deaths ~ day + weekDay + quarter, data = trainSub, family = "poisson")
summary(poissonModelTrain4)

# tidy(poissonModelTrain2) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)



# modele glm dla zbioru testowego - ROZKLAD POISSONA
#day
poissonModelTest = glm(new_deaths ~ day, data = testSub, family = "poisson")
summary(poissonModelTest)

# model regresji -> y =

# model_visual
# tidy(poissonModelTest) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)

ggplot(testSub, aes(y=new_deaths,x=day))+geom_point()+geom_smooth(method="lm")

#quarter
poissonModelTest2 = glm(new_deaths ~ quarter, data = testSub, family = "poisson")
summary(poissonModelTest2)

ggplot(testSub, aes(y=new_deaths,x=quarter))+geom_point()+geom_smooth(method="lm")

#isWeekend - czy jest sens???
# poissonModelTest3 = glm(new_deaths ~ isWeekend, data = testSub, family = "poisson")
# summary(poissonModelTest3)

#wieloczynnikowa
poissonModelTest4 = glm(new_deaths ~ day + weekDay + quarter, data = testSub, family = "poisson")
summary(poissonModelTest4)

# tidy(poissonModelTest4) %>%
#   gather(x, y, estimate:p.value) %>%
#   ggplot(aes(x = term, y = y, color = x, fill = x)) +
#   facet_wrap(~ x, scales = "free", ncol = 4) +
#   geom_bar(stat = "identity", alpha = 0.8)



# predykcja dla danych TRENINGOWYCH -------------------

predTestTrain <- trainSub %>%
  add_predictions(gaussianModelTrain, "pred_lm") %>%
  add_residuals(gaussianModelTrain, "resid_lm")


indexFutureTrain = seq.Date(Sys.Date(), Sys.Date()+30, by = "day")
head(indexFutureTrain)

dataFutureTrain = indexFutureTrain %>%
  tk_get_timeseries_signature()


dataFutureTrain$date <- dataFutureTrain$index
dataFutureTrain$isWeekend <- as.factor(ifelse(dataFutureTrain$wday > 5, "weekend", "week"))
predFutureTrain <- predict(gaussianModelTrain, newdata = dataFutureTrain)


futureDfTrain <- data.frame(date = indexFutureTrain,
                       new_deaths = predFutureTrain)

testResiduals <- predTestTrain$resid_lm
testResidSd <- sd(testResiduals, na.rm = TRUE)

futureDfTrain <- futureDfTrain %>%
  mutate(
    lo.95 = new_deaths - 1.96 * testResidSd,
    hi.95 = new_deaths + 1.96 * testResidSd)

ggplot(futureDfTrain, aes(x = date, y = new_deaths)) +
  geom_line() +
  geom_ribbon(aes(ymin=lo.95, ymax=hi.95), linetype=2, alpha=0.1) 


# model Meanf
'''
as.numeric(min(trainSub$date)-as.Date("2020-01-01"))
timeSeries = ts(trainSub$new_deaths, start = c(2020, as.numeric(min(trainSub$date))), 
                end = c(2020, as.numeric(max(trainSub$date))), 
                frequency = 365.25)
class(timeSeries)
plot(timeSeries)
time(timeSeries)

timeSeries2 = window(timeSeries, end = 2020.794)

timeSeries2 <- timeSeries[1:(length(timeSeries)-2)]
timeSeries2
plot(timeSeries2)

library(forecast)

MAPE = function(y, ypred) {
  mean(abs((y-ypred)/y))*100
}

MAPE(1:10, 2:11)

modelMeanf = meanf(timeSeries, h = nrow(trainSub))
summary(modelMeanf)
plot(trainSub$new_deaths, modelMeanf$mean)

MAPE(trainSub$new_deaths, modelMeanf$mean)
'''