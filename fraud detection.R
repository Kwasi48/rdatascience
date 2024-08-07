# this is a script for Fraudulent Transaction Detection 
#importing the data set into R

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
fraud_data <- fraud_data |> select(!isFlaggedFraud)
fraud_data <- fraud_data |> select(!nameOrig)
fraud_data <- fraud_data |> select(!nameDest)
head(fraud_data)

# which transaction type was used the most 
#ggplot(fraud_data, aes(x=type, )) + geom_bar( mapping = aes(fill = type)) + labs( x= 'Type of Transactions', y = 'Number of Transactions' , title = 'Frequency of Transaction Types')

#ggplot(fraud_data, aes(y=isFraud, x= type)) + geom_point( mapping = aes(fill=isFraud)) 

sum(is.na(fraud_data))
#SETTING THE RANDOM SEED NUMBER
set.seed(123)

fraud_data$isFraud = factor(fraud_data$isFraud, levels = c(0,1))

# Splitting the dataset into the Training set and Test set 
 
library(caTools) 

split = sample.split(fraud_data$isFraud, SplitRatio = 0.75) 

training_set = subset(fraud_data, split == TRUE) 
test_set = subset(fraud_data, split == FALSE) 

view(training_set)
head(test_set)
view(split)

# Fitting Support Vector Machine to the training set 
classifier = svm(formula = isFraud ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 
classifier

model.training <- predict(classifier, training_set)
model.testing <- predict(classifier, test_set)

#confusion matrix
confusion_matrix <- caret::confusionMatrix(model.training, training_set$isFraud)
confusion_matrix1 <- caret::confusionMatrix(test_set$isFraud, model.testing)

confusion_matrix
confusion_matrix1
recall1 <- recall(training_set$isFraud, model.training,)
recall1

recall2 <- ModelMetrics::recall( test_set$isFraud, model.testing, )
recall2

preci <- ModelMetrics::precision( test_set$isFraud,model.testing,)
preci

f1_score <- ModelMetrics::f1Score(model.training, training_set$isFraud)
f1_score


#Logistic Regression
 
 