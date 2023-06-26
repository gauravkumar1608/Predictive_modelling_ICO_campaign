ico_RF <- ico_main # Creating a duplicate dataframe
library(caret)
# Performing one-hot encoding because Random Forest cannot handle feature with more than 53 categories
ico_RF_dummy <- dummyVars( ~ countryRegion + platform , data = ico_RF) # Using dummyVars for one hot encoding
dummy_frame <- data.frame(predict(ico_RF_dummy, ico_RF))# Creating a dataframe of dummy features
library(dplyr)
ico_RF <- cbind(ico_RF, dummy_frame) #combining dummy dataframe with RF dataframe
ico_RF$countryRegion <- NULL # removing country feature from final dataframe
ico_RF$platform <- NULL # removing platform feature from final dataframe
#table(ico_RF$success)
ico_RF$success <- factor(ico_RF$success) #Converting character features to factors
#table(ico_RF$success)
#Separating Training and Test data
smp_size_RF <- floor(0.9 * nrow(ico_RF))
set.seed(12345)
train_ind_RF <- sample(nrow(ico_RF), smp_size_RF)
ico_dt_train_RF <- ico_RF[train_ind_RF, ]
ico_dt_test_RF <- ico_RF[-train_ind_RF, ]
ico_dt_test_labels_RF <- ico_RF[-train_ind_RF, "success"]
ico_dt_test_labels_RF
#Training the model
library(randomForest)
ico_RF_model <- randomForest(success ~ ., data = ico_dt_train_RF, ntree = 20)
#Evaluating the model on test data
ico_RF_predict <- predict(ico_RF_model, ico_dt_test_RF, type = "prob" )
ico_RF_predict1 <- predict(ico_RF_model, ico_dt_test_RF)
#Getting key metrics 
library(ROCR)
predict_object_RF <- prediction(ico_RF_predict[,2],ico_dt_test_labels_RF)
roc_RF <- performance(predict_object_RF, measure = "tpr", x.measure = "fpr")
plot(roc_RF, main = "ROC curve for Random Forest Model", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_RF <- performance(predict_object_RF, measure = "auc")
auc_object_RF
auc_object_RF@y.values[[1]]
#Getting other metrics Accuracy, Sensitivity, Specificity, Precision, F-measure
library(caret)
keymetric_RF <- confusionMatrix(ico_RF_predict1 , ico_dt_test_labels_RF, positive = "Y", mode = "everything")
keymetric_RF