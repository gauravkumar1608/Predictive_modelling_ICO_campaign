#Using the table of Random forest which has been one hot encoded for character features
ico_SVM <- ico_RF
#str(ico_SVM)
#Partitioning the data into training and testing data
smp_size_SVM <- floor(0.9 * nrow(ico_SVM))
set.seed(12345)
train_ind_SVM <- sample(nrow(ico_SVM), smp_size_SVM)
ico_dt_train_SVM <- ico_SVM[train_ind_SVM, ]
ico_dt_test_SVM <- ico_SVM[-train_ind_SVM, ]
ico_dt_test_labels_SVM <- ico_SVM[-train_ind_SVM, "success"]
#ico_dt_test_labels_SVM
# Training the model
#install.packages("kernlab")
library(kernlab)
SVM_model <- ksvm(success ~ ., data = ico_SVM,
                          kernel = "vanilladot", prob.model = TRUE)
?SVM_model
#Evaluating model performance on test data
library(tidyverse)
SVM_predict <- predict(SVM_model, select(ico_dt_test_SVM, -success), type = "probabilities")
SVM_predict1 <- predict(SVM_model, select(ico_dt_test_SVM, -success))
#Getting key metrics 
library(ROCR)
predict_object_SVM <- prediction(SVM_predict[,2],ico_dt_test_labels_SVM)
roc_SVM <- performance(predict_object_SVM, measure = "tpr", x.measure = "fpr")
plot(roc_SVM, main = "ROC curve for SVM Model", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_SVM <- performance(predict_object_SVM, measure = "auc")
auc_object_SVM
auc_object_SVM@y.values[[1]]
#Getting other metrics Accuracy, Sensitivity, Specificity, Precision, F-measure
library(caret)
keymetric_SVM <- confusionMatrix(SVM_predict1 , ico_dt_test_labels_SVM, positive = "Y", mode = "everything")
keymetric_SVM