library(dplyr)
library(rpart)
data<-read.csv(file_path)
data

data_1<-data[,-1]
data_1
data_final<-data_1[,-1]
data_final
fire_data<- data[1:58, ]
fire_data

data_final$Standard<-as.factor(data_final$Standard)
data_final

install.packages('party')
library(party)
library(tidyverse)
library(caret)
install.packages('randomForest')
library(randomForest)
set.seed(123)
training.samples <- data_final$Standard
training.samples <- createDataPartition(1:length(data_final$Standard), p = 0.8, list = FALSE,
                                        group=1)
train_data <- data_final[training.samples,]
test_data <- data_final[-training.samples,]

data_final$Standard<-as.factor(data_final$Standard)
data_final
levels(data_final$Standard)

set.seed(123)
Disaster_Type_RandomForest <- randomForest(Standard~., data = train_data, ntree=800,
                                      mtry=3 ,importance = TRUE)
Disaster_Type_RandomForest
plot(Disaster_Type_RandomForest, main ="Error Plot")
varImpPlot(Disaster_Type_RandomForest)

Disaster_Type_RandomForest$importance

pred_test_randomforest <- predict(object = Disaster_Type_RandomForest, newdata = test_data)
confusionMatrix(pred_test_randomforest,test_data$Standard)
