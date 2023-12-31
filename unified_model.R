rm(list = ls())

source("logistic_regression_pd_analyzer.R")
source("NB_pd_analyser.R")
source("randomForest_pd_analyzer.R")


# Plot the single plot for each seed
single_plots = TRUE

# Choose initial seed
seed = 245

# Load Data
pd_data <- read.csv("parkinsons_disease/pd_speech_features.csv")

set.seed(seed)
# Turn class into logical column
pd_data$class <- as.logical(pd_data$class)

# Get the unique IDs from the 'id' column and 
# Calculate the number of IDs for training and testing
unique_ids <- unique(pd_data$id)                          
n_train <- round(0.9 * length(unique_ids))           
n_test <- length(unique_ids) - n_train               

# Randomly sample the IDs for training and testing 
# and create testing and training dataframes
train_ids <- sample(unique_ids, size = n_train)      
test_ids <- setdiff(unique_ids, train_ids)           
train_pd <- pd_data[pd_data$id %in% train_ids, ]               
test_pd <- pd_data[pd_data$id %in% test_ids, ]   

# Ceate Naive Bayes model predicting pd as function of all patient attributes
# minus their ID
train_selected_columns_pd <- setdiff(names(test_pd), "id")
train_noid_pd <- train_pd[train_selected_columns_pd]

nb_model = nB_model(pd_data, train_ids)
rf_model = rF_model(pd_data, train_ids)
lr_model = lR_model(pd_data, train_ids)

# Confine test model such that it can be used as input for the predict function
selected_columns_pd <- setdiff(names(test_pd), c("id", "class"))
input_test_pd <- test_pd[selected_columns_pd]

# Predict PD for test samples 
nb_test_prediction <- predict(nb_model, input_test_pd)
rf_test_prediction <- predict(rf_model, input_test_pd)
lr_test_prediction <- round(as.numeric(
    predict(lr_model, input_test_pd, type = "response")))

# Checks the importance of each variable in determining the outcome of the prediction
importance_values <- importance(rf_model)
print(importance_values)
varImpPlot(rf_model)

# Analyse Data and compare to actual data
if(single_plots == TRUE){
x11()
plot(seq_len(length(test_pd$class)), as.numeric(test_pd$class),
pch = 1, ylim = c(-0.2, 1.2))

points(seq_len(length(test_pd$class)), as.numeric(nb_test_prediction) - 1,
pch = 3, col = "blue")

points(seq_len(length(test_pd$class)), as.numeric(rf_test_prediction) - 1,
pch = 2, col = "red")

points(seq_len(length(test_pd$class)), as.numeric(lr_test_prediction),
pch = 4, col = "green")

}

# this is for Daniel, the xQuartz package is not working on my end #########
plot(seq_len(length(test_pd$class)), as.numeric(test_pd$class),
     pch = 1, ylim = c(-0.2, 1.2))

points(seq_len(length(test_pd$class)), as.numeric(nb_test_prediction) - 1,
       pch = 3, col = "blue")

points(seq_len(length(test_pd$class)), as.numeric(rf_test_prediction) - 1,
       pch = 2, col = "red")

points(seq_len(length(test_pd$class)), as.numeric(lr_test_prediction),
       pch = 4, col = "green")
############################################################################

# Create combined model
combined_prediction = round((as.numeric(nb_test_prediction) - 1 + 
                       as.numeric(rf_test_prediction) - 1 +
                       as.numeric(lr_test_prediction)) / 3)

# Check the accuracies
nb_accuracy <- sum(nb_test_prediction == test_pd$class) / length(test_pd$class)
rf_accuracy <- sum(rf_test_prediction == test_pd$class) / length(test_pd$class)
lr_accuracy <- sum(lr_test_prediction == test_pd$class) / length(test_pd$class)
combined_accuracy <- sum(combined_prediction == test_pd$class) / length(test_pd$class) # checks accuracy of combined model 

# Finds all the misclassified points for each model 
misclassified_nb <- test_pd[nb_test_prediction != test_pd$class, ]
misclassified_rf <- test_pd[rf_test_prediction != test_pd$class, ]
misclassified_lr <- test_pd[lr_test_prediction != test_pd$class, ]
misclassified_combined <- test_pd[combined_prediction != test_pd$class, ]

library(caret)

# Formats all the predictions to fit confusion matricies
test_pd$class <- factor(test_pd$class, levels = levels(nb_test_prediction))
lr_test_prediction <- as.factor(lr_test_prediction)
levels(lr_test_prediction) <- c("FALSE", "TRUE")
combined_prediction <- as.factor(round(combined_prediction))
levels(combined_prediction) <- c("FALSE", "TRUE")

# Evaluate confusion matrices
conf_matrix_nb <- confusionMatrix(nb_test_prediction, test_pd$class)
conf_matrix_rf <- confusionMatrix(rf_test_prediction, test_pd$class)
conf_matrix_lr <- confusionMatrix(lr_test_prediction, test_pd$class)
conf_matrix_combined <- confusionMatrix(combined_prediction, test_pd$class)

# Print confusion matrices
print("Confusion Matrix for Naive Bayes:")
print(conf_matrix_nb)

print("Confusion Matrix for Random Forest:")
print(conf_matrix_rf)

print("Confusion Matrix for Logistic Regression:")
print(conf_matrix_lr)

print("Confusion Matrix for Combined Model:")
print(conf_matrix_combined)



