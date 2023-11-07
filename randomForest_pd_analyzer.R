# Clear the environment
rm(list = ls())

# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("randomForest")) install.packages("randomForest")

library(tidyverse)
library(randomForest)

# Load the dataset
pd_data <- read.csv("/Users/danielfan/Downloads/bio339n/final project/UTbioinfo/parkinsons_disease/pd_speech_features.csv")

# Convert the 'class' column to logical
pd_data$class <- as.logical(pd_data$class)

# Partition the data into training and testing sets
unique_ids <- unique(pd_data$id)
n_train <- round(0.9 * length(unique_ids))
train_ids <- sample(unique_ids, size = n_train)
train_pd <- pd_data[pd_data$id %in% train_ids, ]
test_pd <- pd_data[!pd_data$id %in% train_ids, ]

# Select the features for training and testing
selected_columns_pd <- setdiff(names(test_pd), c("id", "class"))
train_features <- train_pd[selected_columns_pd]
test_features <- test_pd[selected_columns_pd]

# Select the target variable for training and testing
train_target <- as.factor(train_pd$class)
test_target <- test_pd$class

# Implement Random Forest
rf_model <- randomForest(train_target ~ ., data = train_features, importance = T)

# Make predictions
rf_pred <- predict(rf_model, test_features)
importance(rf_model)
varImpPlot(rf_model)

# Compare the predicted results with the actual data
accuracy <- sum(rf_pred == test_target) / length(test_target) * 100
cat("Accuracy of the Random Forest model:", accuracy, "%\n")

# Accuracy of the Random Forest model: 86.66667 %
