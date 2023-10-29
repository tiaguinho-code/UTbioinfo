library(keras)
library(tensorflow)
library(snedata)
c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_cifar10()
summary(dataset_cifar10())
plot(dataset_cifar10())
dataset_cifar10()[1]
show_cifar(dataset_cifar10())

