#Using the table of Random forest which has been one hot encoded for character features
ico_ANN <- ico_RF
#normalize complete data frame
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
library(dplyr)
ico_ANN_norm <- as.data.frame(lapply(select(ico_ANN, -success) , normalize)) # 'normalize' is the name of function we defined
ico_ANN_norm <- cbind(ico_ANN_norm, success = ico_ANN$success) #combining "success" feature
#summary(ico_ANN_norm)
#Partitioning the data into training and testing data
smp_size_ANN <- floor(0.9 * nrow(ico_ANN_norm))
set.seed(12345)
train_ind_ANN <- sample(nrow(ico_ANN_norm), smp_size_ANN)
ico_dt_train_ANN <- ico_ANN_norm[train_ind_ANN, ]
ico_dt_test_ANN <- ico_ANN_norm[-train_ind_ANN, ]
ico_dt_test_labels_ANN <- ico_ANN_norm[-train_ind_ANN, "success"]
#names(ico_ANN_norm)
#ico_dt_test_labels_ANN
#Training the model
#install.packages("neuralnet")
library(neuralnet)
?neuralnet
set.seed(12345)
ANN_model <- neuralnet(formula = success ~ .,data = ico_ANN_norm)
#plot(ANN_model)
#summary(ANN_model)
#Evaluating the model on test data
ANN_predict <- predict(ANN_model, select(ico_dt_test_ANN, -success))
#ANN gives prediction probabilities; Converting them to class labels
ANN_predict_df <- data.frame(ANN_predict) #Converting prediciton output as a dataframe
class_labels_ANN_test <- ifelse(ANN_predict_df$X2 > 0.5, "Y", "N") #Converting probabilities to class labels
class_labels_ANN_test <- factor(class_labels_ANN_test)

#Getting key metrics 
#install.packages("ROCR")
library(ROCR)
###VERY VERY IMPORTANT###
#1. Remember to load ROCR package again because "prediction" function 
#2. exists in neuralnet package also.
#3. Here, it will first consider prediction from neuralnet and give error
#4. Make sure it is reading prediction from ROCR package
predict_object_ANN <- prediction(ANN_predict_df[,2],ico_dt_test_labels_ANN)
roc_ANN <- performance(predict_object_ANN, measure = "tpr", x.measure = "fpr")
plot(roc_ANN, main = "ROC curve for ANN Model", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_ANN <- performance(predict_object_ANN, measure = "auc")
auc_object_ANN
auc_object_ANN@y.values[[1]]
#Getting other metrics Accuracy, Sensitivity, Specificity, Precision, F-measure
library(caret)
keymetric_ANN <- confusionMatrix(class_labels_ANN_test , ico_dt_test_labels_ANN, positive = "Y", mode = "everything")
keymetric_ANN
