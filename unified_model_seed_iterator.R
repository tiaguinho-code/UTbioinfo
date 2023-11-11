rm(list = ls())

source("NB_pd_analyser.R")
source("randomForest_pd_analyzer.R")
source("logistic_regression_pd_analyzer.R")
source("kNN_pd_analyzer.R")


# Plot the single plot for each seed
single_plots = TRUE

# Choose initial seed
seed = 245
set.seed(seed)

# choose number of iterations
nreps = 8

# create seeds
seeds <- sample(1:100, nreps, replace = TRUE)

if(single_plots){
x11(height = 9, width = 14)
par(mfrow = c(4, 2)) 
}

i = 1
combined_prediction_accuracies = vector(length = nreps)
nb_prediction_accuracies = vector(length = nreps)
rf_prediction_accuracies = vector(length = nreps)
lr_prediction_accuracies = vector(length = nreps)


# Load Data
pd_data <- read.csv("parkinsons_disease/pd_speech_features.csv")

for(seed in seeds){
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
knn_test_prediction <- kNN_model(pd_data, train_ids)

# Analyse Data and compare to actual data
if(single_plots == TRUE){
plot(seq_len(length(test_pd$class)), as.numeric(test_pd$class),
pch = 1, ylim = c(-0.2, 1.2))

points(seq_len(length(test_pd$class)), as.numeric(nb_test_prediction) - 1,
pch = 3, col = "blue")

points(seq_len(length(test_pd$class)), as.numeric(rf_test_prediction) - 1,
pch = 2, col = "red")

points(seq_len(length(test_pd$class)), as.numeric(lr_test_prediction),
pch = 4, col = "green")

# points(seq_len(length(test_pd$class)), as.numeric(knn_test_prediction) - 1,
# pch = 4, col = "#cfc100")

}

# Create combined model
combined_prediction = round((as.numeric(nb_test_prediction) - 1 + 
                       as.numeric(rf_test_prediction) - 1 +
                       as.numeric(lr_test_prediction)) / 3)

# Check the accuracies
nb_accuracy <- sum(nb_test_prediction == test_pd$class) / length(test_pd$class)
rf_accuracy <- sum(rf_test_prediction == test_pd$class) / length(test_pd$class)
lr_accuracy <- sum(lr_test_prediction == test_pd$class) / length(test_pd$class)
combined_accuracy <- sum(combined_prediction == test_pd$class) /
    length(test_pd$class)

nb_prediction_accuracies[i] = nb_accuracy
rf_prediction_accuracies[i] = rf_accuracy
lr_prediction_accuracies[i] = lr_accuracy
combined_prediction_accuracies[i] = combined_accuracy

i = i + 1
}
x11()
plot(combined_prediction_accuracies,
ylim = c(0,1),
xlab = "Seed Iteration",
ylab = "Prediction accuracy",
pch = 1)
points(nb_prediction_accuracies, pch = 3)
points(rf_prediction_accuracies, pch = 2)
points(lr_prediction_accuracies, pch = 4)
legend("bottomright", 
    legend = c("combined", "naive bayes",
               "random forest", "logistic regression"),
    pch = c(1, 3, 2, 4))