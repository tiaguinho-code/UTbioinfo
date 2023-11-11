# Install and load necessary packages
# if (!require("tidyverse")) install.packages("tidyverse")
# if (!require("randomForest")) install.packages("randomForest")

library(tidyverse)
library(randomForest)

# Load the dataset
pd_data <- read.csv("parkinsons_disease/pd_speech_features.csv")
# Open the project with UTBIOINFO as root using setwd(path/to/UTBIOINFO)

rF_model <- function(pd_data, train_ids){
# Convert the 'class' column to logical
pd_data$class <- as.logical(pd_data$class)

# Partition the data into training and testing sets
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

return(rf_model)

}
unique_ids <- unique(pd_data$id)
n_train <- round(0.9 * length(unique_ids))
train_ids <- sample(unique_ids, size = n_train)

rF_model(pd_data, train_ids)
# Accuracy of the Random Forest model: 86.66667 %

