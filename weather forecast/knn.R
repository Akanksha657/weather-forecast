weather_data3
View(weather_data3)
logist<-glm(RainTomorrow~MinTemp,weather_data2,family="binomial")
logist
predict<-predict(logist,MinTemp="7.6")
predict

library(caret)
library(lattice)
library(ggplot2)
set.seed(3033)
intrain <- createDataPartition(weather_data3$MinTemp , p= 0.7, list = FALSE)
training <- weather_data3[intrain,]
testing <- weather_data3[-intrain,]
dim(training); dim(testing)
anyNA(weather_data3)
summary(weather_data3)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
library(e1071)
knn_fit <- train(RainTomorrow ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
test_pred <- predict(knn_fit, newdata = testing)
test_pred

confusionMatrix(test_pred, testing$RainTomorrow )
