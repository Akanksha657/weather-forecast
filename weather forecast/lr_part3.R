#Tuning Analysis
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ROCR))
set.seed(1023)
readRDS(file="wf_log_reg_part2.rds")

#
glm.tune <- function(glm_model, dataset) {
  results <- data.frame()
  for (q in seq(0.2, 0.8, by = 0.02)) {
    fitted_values <- glm_model$finalModel$fitted.values
    prediction <- ifelse(fitted_values >= q, "Yes", "No")                
    cm <- confusionMatrix(prediction, dataset$RainTomorrow)
    accuracy <- cm$overall["Accuracy"]
    specificity <- cm$byClass["Specificity"]
    results <- rbind(results, data.frame(cutoff=q, accuracy=accuracy, specificity = specificity))
  }
  rownames(results) <- NULL
  results
}
glm.tune
mod9am_c1_fit<- glm(RainTomorrow ~ Cloud9am + Humidity9am + Pressure9am,training ,family = "binomial")
mod3pm_c1_fit<- (RainTomorrow ~ Cloud3pm + Humidity3pm )
mod_ev_c2_fit<- (RainTomorrow ~ Cloud3pm + Humidity3pm )
mod_ev_c3_fit<- (RainTomorrow ~ Pressure3pm + Sunshine)
glm.tune(mod9am_c1_fit, training)
View(training)
View(mod9am_c1_fit)
dim (training)
#

opt_cutoff < 0.5;- 
  pred_test <- predict(mod9am_c1_fit, testing, type = "prob")
prediction = ifelse(pred_test$Yes >= opt_cutoff, "Yes", "No")
confusionMatrix(prediction, testing$RainTomorrow)
