rm(list = ls())

library(tidyverse)
library(neuralnet)
library(ggplot2)
library(rgl)

set.seed(245)
data_rows <- floor(0.80 * nrow(iris))
train_indices <- sample(c(seq_along(nrow(iris))), data_rows)
train_data <- iris[train_indices,]
test_data <- iris[-train_indices,]

model = neuralnet(
    Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
data = train_data,
hidden = c(5, 2),
linear.output = FALSE
)


#plot(model, rep = "best")
#plot3d(iris$Sepal.Length, iris$Sepal.Width, 
#         iris$Petal.Length, col = c("red", "green", "blue")[iris$Species])

pred <- predict(model, test_data)
labels <- c("setosa", "versicolor", "virginca")
prediction_label <- data.frame(max.col(pred)) %>%     
mutate(pred = labels[max.col.pred.]) %>%
select(2) %>%
unlist()

table(test_data$Species, prediction_label)

check = as.numeric(test_data$Species) == max.col(pred)
accuracy = (sum(check) / nrow(test_data)) * 100
print(accuracy)


