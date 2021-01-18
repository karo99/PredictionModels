library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(tidyquant)
library(broom)
library(timekit)
library(modelr)

data = read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
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


# modele glm dla zbioru treningowego
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



# modele glm dla zbioru testowego
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

