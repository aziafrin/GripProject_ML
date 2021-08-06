rm(list=ls())


#READING DATA
score_data=read.table("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv",header=TRUE,sep=",",dec=".")
as.data.frame(score_data)

head(score_data)
summary (score_data)

#PLOTING DATA
plot(score_data,xlab="Study Hours of Students",ylab="Score of Sudents")

#CORRELATION
cor(score_data$Hours,score_data$Scores)

#SPLIT
set.seed(10000)
library(lattice)
library(ggplot2)
library(caret)
library(tidyverse)
y<-score_data$Scores
train_index <- createDataPartition(y, times = 1, p = 0.80, list = FALSE)

train_set <- score_data %>% slice(train_index)
train_set
test_set <- score_data %>% slice(-train_index)
test_set


avg <- mean(train_set$Scores)
avg

mean((avg-(test_set$Scores))^2)

#LINEAR REGRESSION MODEL
fit <- lm(Scores ~Hours, data = train_set)
fit$coef


predicted_scores <- predict(fit,test_set)

as.data.frame(predicted_scores)

actual_scores <-test_set$Scores

#SEE VALUES

cbind(predicted_scores,actual_scores)

#CALCULATE ROOT MEAN SQUARED ERROR

rmse<-mean((actual_scores-predicted_scores)^2)
rmse

#PLOT AGAIN
plot(score_data,xlab="Study Hours of Students",ylab="Score of Sudents")
abline(fit,col="blue")

#CHECK MODEL
new <- data.frame(Hours=c(9.25))
predict(fit, newdata = new)

