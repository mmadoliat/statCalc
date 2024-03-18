library(keras)
reticulate::install_python()
library(tensorflow)
install_tensorflow(extra_packages = "keras")



library(reticulate)
virtualenv_create(envname = "r-tensorflow")

c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_mnist()
#mnist <- dataset_mnist()
#x_train <- mnist$train$x
#y_train <- mnist$train$y
#x_test <- mnist$test$x
#y_test <- mnist$test$y
#train_images <- mnist$train$x
train_labels <- mnist$train$y

#TRAIN
size <- 10
image1 <- sample(1:60000,size,replace=T)
image2 <- sample(1:60000,size,replace=T)

x_train <-cbind(x_train[image1,,], x_train[image2,,])
y_train <-as.numeric(paste0(y_train[image1], y_train[image2]))
print(y_train[2])

#TEST
size <- 10
image1Test <- sample(1:10000,size,replace=T)
image2Test <- sample(1:10000,size,replace=T)

x_test <-cbind(x_test[image1Test,,], x_test[image2Test,,])
y_test <-as.numeric(paste0(y_test[image1Test], y_test[image2Test]))
print(y_test[2])





