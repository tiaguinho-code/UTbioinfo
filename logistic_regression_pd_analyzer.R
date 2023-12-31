library(tidyverse)
library(caret)
library(ggplot2)
set.seed(254)

# Load and partition Parkinsons Data
pd_data = read.csv("parkinsons_disease/pd_speech_features.csv")

lR_model <- function(pd_data, train_ids){
# Turn class (Wheter parkinsons or not) into logical column
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

# Ceate LR model predicting pd as function of all patient attributes
# minus their ID 
train_selected_columns_pd <- setdiff(names(test_pd), "id")
train_noid_pd <- train_pd[train_selected_columns_pd]

lr_pd_model <- glm(class ~ ., data = train_noid_pd, family = binomial())

return(lr_pd_model)
}


# Turn class (Wheter parkinsons or not) into logical column
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

lr_pd_model = lR_model(pd_data, train_ids)

# Ceate LR model predicting pd as function of all patient attributes
# minus their ID 
train_selected_columns_pd <- setdiff(names(train_pd), "id")
train_noid_pd <- train_pd[train_selected_columns_pd]


# Confine test model such that it can be used as input for the predict function
selected_columns_pd <- setdiff(names(test_pd), c("id", "class"))
input_test_pd <- test_pd[selected_columns_pd]

# Predict PD for test samples with logistic regression 
test_prediction <- predict(lr_pd_model, input_test_pd, type = "response")


# x11()
plot(seq_len(length(test_pd$class)), as.numeric(test_prediction),
pch = 3)#, ylim = c(-0.2, 1.2))

points(seq_len(length(test_pd$class)), as.numeric(test_pd$class),
pch = 1)

