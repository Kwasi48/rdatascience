library(tidyverse)
#library('randomForest')
library('caret')
library('e1071')

fraud_data <- read.csv("C:/Users/mike/Downloads/financial dataset/Synthetic_Financial_datasets_log.csv")
head(fraud_data)
tibble(fraud_data)

#fraud_data_dec <- fraud_data |> select(!oldbalanceOrg | newbalanceOrig, oldbalanceDest, newbalanceDest )
fraud_data <- fraud_data |> select(!oldbalanceOrg)
fraud_data <- fraud_data |> select(!newbalanceOrig)
fraud_data <- fraud_data |> select(!oldbalanceDest)
fraud_data <- fraud_data |> select(!newbalanceDest )
fraud_data <- fraud_data |> select(!step)
fraud_data <- fraud_data |> select(!isFraud)
fraud_data <- fraud_data |> select(!nameOrig)
fraud_data <- fraud_data |> select(!nameDest)
fraud_data <- fraud_data |> select(!type)
head(fraud_data)

fraud_analysis <- fraud_data %>%  select(amount, isFraud)
fraud_analysis

library("caTools")

#install.packages("ROCR")
library(ROCR)

fraud_analysis$isFraud = factor(fraud_analysis$isFraud, levels = c(0,1))

 

splitit <- sample.split( fraud_analysis$isFraud, SplitRatio = 0.8)
splitit

train_set <- subset(fraud_analysis, split == TRUE)
test_set <- subset(fraud_analysis, split == FALSE)

ggplot(fraud_analysis, aes( x= amount , y= isFraud)) + geom_jitter( height = 1, alpha =1)

logistic_regre <- glm(isFraud ~ amount, data = train_set, family = "binomial")

summary(logistic_regre)

anova(logistic_regre, test="Chisq")

predict.result <- predict(logistic_regre, test_set, type = "response")
predict.result <- ifelse(predict.result > 0.5, 1, 0)

misClassError <- mean(predict.result != fraud_analysis$isFraud)
print(paste("Accuracy",1-misClassError))



pred.result <- predict(logistic_regre, test_set, type = "response")
pre.result <- prediction(pred.result, fraud_analysis$isFraud)
perf <- performance(pre.result, measure = "tpr", x.measure = 'fpr')
plot(perf)

auc <- performance(pre.result, measure = "auc")
auc <- auc@y.values[[1]]
auc
