library(tidyverse)
library('randomForest')
library('caTools')
#library('e1071')

fraud_data <- read.csv("C:/Users/mike/Downloads/financial dataset/Synthetic_Financial_datasets_log.csv")
head(fraud_data)
tibble(fraud_data)

#fraud_data_dec <- fraud_data |> select(!oldbalanceOrg | newbalanceOrig, oldbalanceDest, newbalanceDest )
fraud_data <- fraud_data |> select(!oldbalanceOrg)
fraud_data <- fraud_data |> select(!newbalanceOrig)
fraud_data <- fraud_data |> select(!oldbalanceDest)
fraud_data <- fraud_data |> select(!newbalanceDest )
fraud_data <- fraud_data |> select(!step)
fraud_data <- fraud_data |> select(!isFlaggedFraud)
fraud_data <- fraud_data |> select(!nameOrig)
fraud_data <- fraud_data |> select(!nameDest)
view(fraud_data)
str(fraud_data)
head(fraud_data)

fraud_data$isFraud = factor(fraud_data$isFraud, levels = c(0,1))

#splitting the data 
splitz <- sample.split(fraud_data, SplitRatio = 0.8)

#.....into test and train set
set_train <- subset(fraud_data, splitz == "TRUE")
set_test <- subset(fraud_data, splitz == "FALSE")

#Fitting Random Forest to the train set
set.seed(100)
head(set_train[-1])
gc()
model.ran <- randomForest(x = set_train[-1], y = set_train$isFraud, ntree = 500)
model.ran

#predicting the test set result
pred_test <- predict(model.ran, newdata = set_test[-1])
