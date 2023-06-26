ico_dt <- ico_main
str(ico_main)
#Converting character features to factors
ico_dt[,c("success", "countryRegion", "platform")] <- lapply(ico_dt[,c("success", "countryRegion", "platform")], factor)
str(ico_dt)
#Separating Training and Test data
smp_size <- floor(0.9 * nrow(ico_dt))
set.seed(12345)
train_ind <- sample(nrow(ico_dt), smp_size)
ico_dt_train <- ico_dt[train_ind, ]
ico_dt_test <- ico_dt[-train_ind, ]
ico_dt_test_labels <- ico_dt[-train_ind, "success"]
ico_dt_test_labels
# Checking the distribution of classes in training and testing data
prop.table(table(ico_dt_train$success))
prop.table(table(ico_dt_test $success))
#Training the model
library(C50)
library(tidyverse)
dt_model <- C5.0(success ~ ., ico_dt_train, rules = TRUE)
dt_model
summary(dt_model)
#Evaluating the performance of DT model
dt_pred <- predict(dt_model, ico_dt_test, type = "prob" )
dt_pred1 <- predict(dt_model, ico_dt_test)
dt_pred
#Developing ROC Curve
library(ROCR)
predict_object <- prediction(dt_pred[,2],ico_dt_test_labels)
roc_DT <- performance(predict_object, measure = "tpr", x.measure = "fpr")
plot(roc_DT, main = "ROC curve for Decision Tree Model", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_DT <- performance(predict_object, measure = "auc")
auc_object_DT
auc_object_DT@y.values[[1]]
#Getting other metrics Accuracy, Sensitivity, Specificity, Precision, F-measure
library(caret)
#?confusionMatrix
keymetric_DT <- confusionMatrix(dt_pred1 , ico_dt_test_labels, positive = "Y", mode = "everything")
keymetric_DT