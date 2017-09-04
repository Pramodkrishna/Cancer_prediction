getwd()
setwd("Documents/Kaggle/Cancer/")
install.packages("corrplot")
install.packages("caret")
library(class)
library(dplyr)
library(e1071)
library(corrplot)
library(caret)

s.data<- read.csv("data.csv",header = T,stringsAsFactors = F)
head  (s.data)
s.data[,33] <- NULL

summary(s.data)
s.data$diagnosis <- as.factor(s.data$diagnosis)

corr_mat <- cor(s.data[,3:ncol(s.data)])
corrplot(corr_mat)


#70 - 30
data_index1 <- createDataPartition(s.data$diagnosis, p=0.7, list = FALSE)
train_data1 <- s.data[data_index1, -1]
test_data1 <- s.data[-data_index1, -1]


#60-40
data_index2 <- createDataPartition(s.data$diagnosis, p=0.6, list = FALSE)
train_data2 <- s.data[data_index2, -1]
test_data2 <- s.data[-data_index2, -1]


#Perfomr Linear modelling 

s.data1 <- train(diagnosis ~ ., data = train_data1,method="lda2")

summary(s.data1)

s.data1_pred <- predict(s.data1,test_data1)

s.data1_cm <- confusionMatrix(s.data1_pred,test_data1$diagnosis,positive = "M")

s.data1_cm


#Perform SVM
