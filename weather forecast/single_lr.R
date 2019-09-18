suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(gmodels))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(Kmisc))
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(corrplot))



set.seed(1023)
# import data
weather_data <- read.csv("G:/r programing/weather forecast/weather.csv")
# view first 6 rows of data set
head(weather_data)
# view column names of data set
colnames(weather_data)
# structure of the dataset
str(weather_data)
#
(n <- nrow(weather_data))
# span of timeline
c(as.character(weather_data$Date[1]), as.character(weather_data$Date[n]))
# RISK_MM relation with the RainTomorrow 
all.equal(weather_data$RISK_MM > 1, weather_data$RainTomorrow == "Yes")
# Rainfall relation with the RainTomorrow 
all.equal(weather_data$Rainfall > 1, weather_data$RainToday == "Yes")
# new subset excluding date location risk_mm rainfall rainfall today
weather_data2 <- subset(weather_data, select = -c(Date, Location, RISK_MM, Rainfall, RainToday))
# view column names of 2nd data set
colnames(weather_data2)
# na replaced with 0
(cols_withNa <- apply(weather_data2, 2, function(x) sum(is.na(x))))
#considering the complete cases in 3rd dataset
weather_data3 <- weather_data2[complete.cases(weather_data2),]
#categorical 
factor_vars <- names(which(sapply(weather_data3, class) == "factor"))
factor_vars <- setdiff(factor_vars, "RainTomorrow")
chisq_test_res <- lapply(factor_vars, function(x) { 
  chisq.test(weather_data3[,x], weather_data3[, "RainTomorrow"], simulate.p.value = TRUE)
})
names(chisq_test_res) <- factor_vars
chisq_test_res

library(gmodels)
library(dplyr)
library(mosaic)
library(ggplot2)
barchart_res <- lapply(factor_vars, function(x) { 
  title <- colnames(weather_data3[,x, drop=FALSE])
  wgd <- CrossTable(weather_data3[,x], weather_data3$RainTomorrow, prop.chisq=F)
  barchart(wgd$prop.row, stack=F, auto.key=list(rectangles = TRUE, space = "top", title = title))
})
names(barchart_res) <- factor_vars
barchart_res$WindGustDir
barchart_res$WindDir9am
barchart_res$WindDir3pm

weather_data4 <- subset(weather_data2, select = -c(WindDir9am, WindDir3pm))
weather_data5 <- weather_data4[complete.cases(weather_data4),]
colnames(weather_data5)


factor_vars <- names(which(sapply(weather_data5, class) == "factor"))
numeric_vars <- setdiff(colnames(weather_data5), factor_vars)
numeric_vars <- setdiff(numeric_vars, "RainTomorrow")
numeric_vars
numeric_vars_mat <- as.matrix(weather_data5[, numeric_vars, drop=FALSE])
numeric_vars_cor <- cor(numeric_vars_mat)
library(corrplot)
corrplot(numeric_vars_cor)

pairs(weather_data5[,numeric_vars], col=weather_data5$RainTomorrow)

library(knitr )
library(dplyr)
gp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=weather_data5, aes(x= RainTomorrow, y=eval(parse(text=x)), col = RainTomorrow)) + geom_boxplot() + xlab("RainTomorrow") + ylab(x) + ggtitle("") + theme(legend.position="none")}))
grob_plots <- invisible(lapply(chunk(1, length(gp), 4), function(x) {
  marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
grob_plots

#
gp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=weather_data5, aes(x=eval(parse(text=x)), col = RainTomorrow)) + geom_density() + xlab(x) + ggtitle(paste(x, "density", sep= " "))}))
grob_plots <- invisible(lapply(chunk(1, length(gp), 4), function(x) {
  marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
grob_plots

#
write.csv(weather_data5, file="weather_data5.csv", sep=",", row.names=FALSE)
write.csv(weather_data3, file="weather_data3.csv", sep=",", row.names=FALSE)
