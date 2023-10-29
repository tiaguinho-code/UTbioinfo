# This script tries tha analysis of pd with the naive bayes model
# and offers a model that can be used to attempt to predict PD in
# a patient

rm(list = ls())

library(tidyverse)
library(naivebayes)
library(ggplot2)
library(rgl)
set.seed(245)

# Load and partition Parkinsons Data
pd_data = read.csv("parkinsons_disease/pd_speech_features.csv")

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

# Ceate Naive Bayes model predicting pd as function of all patient attributes
# minus their ID
train_selected_columns_pd <- setdiff(names(test_pd), "id")
train_noid_pd <- train_pd[train_selected_columns_pd]

nbmodel_pd <- naive_bayes(class ~ . , data = train_noid_pd)

# Confine test model such that it can be used as input for the predict function
selected_columns_pd <- setdiff(names(test_pd), c("id", "class"))
input_test_pd <- test_pd[selected_columns_pd]

# Predict PD for test samples 
test_prediction <- predict(nbmodel_pd, input_test_pd)

# Analyse Data and compare to actual data
x11()
plot(seq_len(length(test_pd$class)), as.numeric(test_prediction) - 1,
pch = 3, ylim = c(-0.2, 1.2))

points(seq_len(length(test_pd$class)), as.numeric(test_pd$class),
pch = 1)
