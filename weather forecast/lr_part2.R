suppressPackageStartupMessages(library(caret))
set.seed(1023)
weather_data5 <- read.csv("weather_data5.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
colnames(weather_data5)
#
nrow(weather_data5)
#
sum(weather_data5["RainTomorrow"] == "Yes")
#
sum(weather_data5["RainTomorrow"] == "No")
# original dataset in a training dataset (70% of original data) and a testing dataset (30% remaining)
train_rec <- createDataPartition(weather_data5$RainTomorrow, p = 0.7, list = FALSE)
training <- weather_data5[train_rec,]
testing <- weather_data5[-train_rec,]
#check the balance of RainTomorrow Yes/No fractions in the training and testing datasets
sum(training["RainTomorrow"] == "Yes")/sum(training["RainTomorrow"] == "No")
sum(testing["RainTomorrow"] == "Yes")/sum(testing["RainTomorrow"] == "No")

#9AM Forecast Model
trControl <- trainControl(method = "repeatedcv",  repeats = 5, number = 10, verboseIter = FALSE)
#1
predictors_9am_c1 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "Temp9am")
formula_9am_c1 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c1, collapse="+"), sep="~"))
mod9am_c1_fit <- train(formula_9am_c1,  data=training, method="glm", 
                       family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c1_fit$results$Accuracy
(summary_rep <- summary(mod9am_c1_fit$finalModel))
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)
drop1(mod9am_c1_fit$finalModel, test="Chisq")
#2
predictors_9am_c2 <- c("Cloud9am",  "Humidity9am", "Pressure9am", "MinTemp")
formula_9am_c2 <- as.formula(paste("RainTomorrow", paste(predictors_9am_c2, collapse="+"), sep="~"))
mod9am_c2_fit <- train(formula_9am_c2,  data=training, method="glm", 
                       family="binomial", trControl = trControl, metric = 'Accuracy')
mod9am_c2_fit$results$Accuracy
(summary_rep <- summary(mod9am_c2_fit$finalModel))
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

mod9am_pred <- predict(mod9am_c1_fit, testing)
confusionMatrix(mod9am_pred, testing[,"RainTomorrow"])

#model2 3PM Forecast Model
predictors_3pm_c1 <- c("Cloud3pm", "Humidity3pm", "Pressure3pm", "Temp3pm")
formula_3pm_c1 <- as.formula(paste("RainTomorrow", paste(predictors_3pm_c1, collapse="+"), sep="~"))
mod3pm_c1_fit <- train(formula_3pm_c1,  data = training, method = "glm", family = "binomial",
                       trControl = trControl, metric = 'Accuracy')
mod3pm_c1_fit$results$Accuracy
(summary_rep <- summary(mod3pm_c1_fit$finalModel))
drop1(mod3pm_c1_fit$finalModel, test="Chisq")
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

#
mod3pm_pred <- predict(mod3pm_c1_fit, testing)
confusionMatrix(mod3pm_pred, testing[,"RainTomorrow"])

#Evening Forecast Model
predictors_evening_c1 <- c("Pressure3pm", "Temp3pm", "Sunshine")
formula_evening_c1 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c1, collapse="+"), sep="~"))
mod_ev_c1_fit <- train(formula_evening_c1,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c1_fit$results$Accuracy

#final model
(summary_rep <- summary(mod_ev_c1_fit$finalModel))
drop1(mod_ev_c1_fit$finalModel, test="Chisq")
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

#As a second tentative model, we take advantage of the 3PM model predictors together with WindGustDir and WindGustSpeed.

predictors_evening_c2 <- c(predictors_3pm_c1, "WindGustDir", "WindGustSpeed")
formula_evening_c2 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c2, collapse="+"), sep="~"))
mod_ev_c2_fit <- train(formula_evening_c2,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c2_fit$results$Accuracy

(summary_rep <- summary(mod_ev_c2_fit$finalModel))
drop1(mod_ev_c2_fit$finalModel, test="Chisq")

#revised model
predictors_evening_c2 <- c(predictors_3pm_c1, "WindGustDir")
formula_evening_c2 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c2, collapse="+"), sep="~"))
mod_ev_c2_fit <- train(formula_evening_c2,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c2_fit$results$Accuracy
(summary_rep <- summary(mod_ev_c2_fit$finalModel))
drop1(mod_ev_c2_fit$finalModel, test="Chisq")
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

#To investigate a final third choice,gather a small set of predictors, Pressure3pm and Sunshine.
predictors_evening_c3 <- c("Pressure3pm", "Sunshine")
formula_evening_c3 <- as.formula(paste("RainTomorrow", paste(predictors_evening_c3, collapse="+"), sep="~"))
mod_ev_c3_fit <- train(formula_evening_c3,  data=training, method="glm", family="binomial", trControl = trControl, metric = 'Accuracy')
mod_ev_c3_fit$results$Accuracy
(summary_rep <- summary(mod_ev_c3_fit$finalModel))
drop1(mod_ev_c3_fit$finalModel, test="Chisq")
1 - pchisq(summary_rep$deviance, summary_rep$df.residual)

#compare the last two models by running an ANOVA analysis on those to check if the lower residual deviance of the first model is significative or not
anova(mod_ev_c2_fit$finalModel, mod_ev_c3_fit$finalModel, test="Chisq")

modevening_pred <- predict(mod_ev_c2_fit, testing)
confusionMatrix(modevening_pred, testing[,"RainTomorrow"])

modevening_pred <- predict(mod_ev_c3_fit, testing)
confusionMatrix(modevening_pred, testing[,"RainTomorrow"])

saveRDS(list(weather_data5, train_rec, training, testing, mod9am_c1_fit, mod9am_c2_fit, mod3pm_c1_fit, mod_ev_c2_fit, mod_ev_c3_fit), file="wf_log_reg_part2.rds")


#Tuning Analysis
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ROCR))
set.seed(1023)
