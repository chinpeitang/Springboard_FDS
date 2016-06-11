# Predicting the quality of wine
# Available at YouTube: https://www.youtube.com/watch?v=vI3envXmyDs
wine <- read.csv("wine.csv")
str(wine)
summary(wine)
# AGST - average growing season temperature
model1 = lm(Price ~ AGST, data=wine)
summary(model1)
# check the residuals of each observation
model1$residuals
SSE1 = sum(model1$residuals^2)
SSE1

# add one more independent variables
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
SSE2 = sum(model2$residuals^2)
SSE2

# add all of the independent variables
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
SSE3 = sum(model3$residuals^2)
SSE3

# Age and France population are insignificant
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
SSE4 = sum(model4$residuals^2)
SSE4

# Correlation Analysis
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
