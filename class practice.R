setwd("C:/Users/user/OneDrive/Documents/Academic/STAC67/R")
senic <- read.csv("APPENC01.txt", sep = "", header = FALSE)
colnames(senic) <- c("iden","length","age","infection","culturing", "Xray",
                     "beds","medschool","region","census","nurses","facilities")

attach(senic)

#Model 1
pairs(senic[, c("length", "age", "infection", "facilities")])
cor(senic[, c("length", "age", "infection", "facilities")])

#Model 2
pairs(senic[,c("length", "beds", "infection", "facilities")])
cor(senic[,c("length", "beds", "infection", "facilities")])

#c Fit the two different model
# if you add a 0 i.e. length ~ 0 + age + ..., then there's no intersection
#lm creates a list
fit1 <- lm(length ~ age + infection + facilities, data = senic)
fit2 <- lm(length ~ beds + infection + facilities, data = senic)

summary(fit1)
summary(fit2)

#keep the first model, comparing with the two models
# because in the first model, we have 3 significant p values


#e Is there a significant linear relationship between length of stay and the three independent variable?
fitted_var <- fitted.value()

#f
yhat <- fitted(fit1)
# OR
yhat2 <- fit1$fitted.values
e <- residuals(fit1)
#OR
e2 <- fit1$residuals
plot(yhat, e)
plot(fit1)
#quite close to a straight line


#g Gauss-Markov Assumption satisfied?
# Not so sure. Not a perfect linear model



#h
newdata <- data.frame(age = 55, infection = 3, facilities = 50)
predict(fit1, newdata, interval = "confidence", level = 0.90)

#i
predict(fit1, newdata, interval = "prediction", level = 0.90)

