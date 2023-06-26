#Model based on Adaptive Boosting
dt_boost <- C5.0(select(ico_dt_train, -success), ico_dt_train$success, trails = 10)
dt_boost
summary(dt_boost)

dt_boost_pred <- predict(dt_boost, ico_dt_test, type = "prob")
dt_boost_pred1 <- predict(dt_boost, ico_dt_test )

#library(ROCR)
predict_object_boost<- prediction(dt_boost_pred[,2],ico_dt_test_labels)
roc_DT_boost <- performance(predict_object_boost, measure = "tpr", x.measure = "fpr")
plot(roc_DT_boost, main = "ROC curve for Adaptive Boosting Model", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_boost <- performance(predict_object_boost, measure = "auc")
auc_object_boost
auc_object_boost@y.values[[1]]
#Getting other metrics Accuracy, Sensitivity, Specificity, Precision, F-measure
#library(caret)

keymetric_Ada <- confusionMatrix(dt_boost_pred1 , ico_dt_test_labels, positive = "Y", mode = "everything")
keymetric_Ada