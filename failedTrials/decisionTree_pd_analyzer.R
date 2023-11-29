# Clear the environment
rm(list = ls())

# Install and load necessary packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("rpart")) install.packages("rpart")

library(tidyverse)
library(rpart)

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

# Implement Decision Tree
dt_model <- rpart(train_target ~ ., data = train_features, method = "class")

# Make predictions
dt_pred <- predict(dt_model, test_features, type = "class")

# Compare the predicted results with the actual data
accuracy <- sum(dt_pred == test_target) / length(test_target) * 100
cat("Accuracy of the Decision Tree model:", accuracy, "%\n")

# Accuracy of the Decision Tree model: 72 %

# Plot the decision tree
summary(dt_model)
plot(dt_model)
text(dt_model,pretty = 0)
