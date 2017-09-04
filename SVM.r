
getwd()
setwd("Documents/Kaggle/Cancer/")
install.packages("gridExtra") 
library(gridExtra)
library(e1071)
library(ggplot2)
library(dplyr)

cancer_data <- read.csv("data.csv",header = T,stringsAsFactors = T)

cancer_data_na <- cancer_data[,-33]

cancer_diagnosis <- subset(cancer_data, select=-diagnosis)
diagnosis  <- cancer_data$diagnosis

#svm_model <- svm(Species ~ ., data=iris)
svm_cancer <- svm(diagnosis~.,data = cancer_data_na)
svm_cancer
#prediction 

cancer_pred <- predict(svm_cancer,cancer_data_na)
cancer_pred
#Applying the prediction model on the data set
Cancer_CM <- table(cancer_pred,diagnosis)

"""Qplot visualization""" 

a <- qplot(cancer_pred,col= "red")
b <- qplot(cancer_data$diagnosis)
grid.arrange(a,b, ncol=2)





"""
Trying the model on a test data frame obtained from breaking the source data into data frames and test the model and 
see the accuracy

"""


#Breaking the data set

## 75% of the sample size
split_value <- floor(0.75 * nrow(cancer_data_na))

split_data  <- sample(seq_len(nrow(cancer_data_na)), size = split_value)

cancer_train <- cancer_data_na[split_data, ]
cancer_test <- cancer_data_na[-split_data, ]
train_diag <- cancer_train$diagnosis
View(cancer_test)

length(train_diag)

svm_cancer1 <- svm(train_diag~.,data = cancer_train)

cancer_pred1 <- predict(svm_cancer1,cancer_train)
cancer_pred1

Cancer_CM1 <- table(cancer_pred1,train_diag)
Cancer_CM1

"""Qplot visualization""" 

a1 <- qplot(cancer_pred1,col= "red")
b1 <- qplot(cancer_train$diagnosis)
grid.arrange(a1,b1, ncol=2)

