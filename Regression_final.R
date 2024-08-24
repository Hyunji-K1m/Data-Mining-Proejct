library(dplyr)
library(readxl)
library(tidyverse)
library(caret)

#Colombia
Colombia<-read_excel("/Users/kimhyunji/Desktop/Data Mining and Analytics/Group Project/Colombia_final.xlsx")
Colombia

set.seed(123)
training.samples <- Colombia$Deaths
training.samples <- createDataPartition(1:length(Colombia$Deaths), p = 0.8, list = FALSE,
                                          group=1)
train_data <- Colombia[training.samples,]
test_data <- Colombia[-training.samples,]

col_reg1 = lm(Deaths ~ Injured, data = train_data)
summary(col_reg1)

col_reg2 = lm(Deaths ~ Injured+Missing, data = train_data)
summary(col_reg2)

col_reg3 = lm(Deaths ~ Evacuated+`Houses Destroyed`+`Indirectly Affected`+`With Missing`+`Directly affected`, data=train_data)
summary(col_reg3)


pred1tr <- col_reg3 %>% predict(train_data)
RMSE1 = RMSE(pred1tr, train_data$Deaths)
R2_1 = R2(pred1tr, train_data$Deaths)

pred1test <- col_reg3 %>% predict(test_data)
RMSE2 = RMSE(pred1test, test_data$Deaths)
R2_2 = R2(pred1test, test_data$Deaths)

table <- matrix(c(RMSE1,RMSE2,R2_1,R2_2),ncol=4,byrow=TRUE)
colnames(table) <- c("RMSE Train"," RMSE Test","R2 Train"," R2 Test")
rownames(table) <- c("Model 1")
RMSE_R2_table <- as.table(table)
RMSE_R2_table






#Ecuador
Ecuador<-read_excel("/Users/kimhyunji/Desktop/Data Mining and Analytics/Group Project/Ecuador_final.xlsx")
Ecuador

set.seed(123)
training_1.samples <- Ecuador$Deaths
training_1.samples <- createDataPartition(1:length(Ecuador$Deaths), p = 0.8, list = FALSE,
                                        group=1)
train_data_1 <- Ecuador[training_1.samples,]
test_data_1 <- Ecuador[-training_1.samples,]

ecu_reg1 = lm(Deaths ~ Injured, data = train_data_1)
summary(ecu_reg1)

ecu_reg3 = lm(Deaths ~ Evacuated+`Houses Damaged`+`Houses Destroyed`+`Directly affected`+`Indirectly Affected`+`With Missing`, data=train_data)
summary(ecu_reg3)


pred1tr_1 <- ecu_reg3 %>% predict(train_data_1)
RMSE1_1 = RMSE(pred1tr_1, train_data_1$Deaths)
R2_1_1 = R2(pred1tr_1, train_data_1$Deaths)

pred1test_1 <- ecu_reg3 %>% predict(test_data_1)
RMSE2_1 = RMSE(pred1test_1, test_data_1$Deaths)
R2_2_1 = R2(pred1test_1, test_data_1$Deaths)

table_1 <- matrix(c(RMSE1_1,RMSE2_1,R2_1_1,R2_2_1),ncol=4,byrow=TRUE)
colnames(table_1) <- c("RMSE Train"," RMSE Test","R2 Train"," R2 Test")
rownames(table_1) <- c("Model 1")
RMSE_R2_table_1 <- as.table(table_1)
RMSE_R2_table_1

# Create a vector of values
values <- c(10, 20, 30, 40)

# Create a vector of labels for the pie chart
labels <- c("Category A", "Category B", "Category C", "Category D")

# Create a pie chart
pie(values, labels = labels, main = "Pie Chart Example")

   