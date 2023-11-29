# Install and load necessary packages
# if (!require("tidyverse")) install.packages("tidyverse")
# if (!require("class")) install.packages("class")
# install.packages("XQuartz")

library(tidyverse)
library(class)
set.seed(3725)

# Load the dataset
pd_data <- read.csv("parkinsons_disease/pd_speech_features.csv") 
# Open the project with UTBIOINFO as root using setwd(path/to/UTBIOINFO)
kNN_model = function(pd_data, train_ids){
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
train_target <- train_pd$class

# Implement KNN
k_value <- 200  # Set the value of k
knn_pred <- knn(train_features, test_features, train_target, k = k_value)
return(knn_pred)
}
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
train_target <- train_pd$class
test_target <- test_pd$class


knn_pred = kNN_model(pd_data, train_ids)

# Compare the predicted results with the actual data

plot(seq_along(test_target), as.numeric(knn_pred) - 1,pch = 3,
ylim = c(-0.2, 1.2),xlab = "Sample Index",
ylab = "Predicted vs Actual PD (KNN)")

points(seq_along(test_target), as.numeric(test_target),
pch = 1,
col = "blue")

legend("topright", legend = c("Predicted PD", "Actual PD"),
col = c("black", "blue"),
pch = c(3, 1))

accuracy <- sum(knn_pred == test_target) / length(test_target) * 100

cat("Accuracy of the KNN model:", accuracy, "%\n")

# Accuracy of the KNN model: 72 %









