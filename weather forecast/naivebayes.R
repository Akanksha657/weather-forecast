library(e1071)
library(ISLR)

# rainfall tomorrow
data1 <- data.frame(read.csv("G:/r programing/weather forecast/weather_data3.csv"))
ran<- sample(1:nrow(data1),0.9 * nrow(data1))
train<-data1[ran,]
test<- data1[-ran,]
attach(data1)
target<- as.factor(data1[ran,9])
nb_default<-naiveBayes(target~.,data=train[,-9])
default_pred<-predict(nb_default,test,type="class")
summary(nb_default)
table(default_pred, test$RainTomorrow ,dnn =c("Prediction","Actual"))

#rainfall today
