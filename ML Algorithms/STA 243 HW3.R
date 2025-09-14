# Preprocessing Dataset------------
install.packages("readmnist")
library(readmnist)

image_train <- Read.mnist('C:/Users/minjk/Downloads/train-images.idx3-ubyte')
label_train <- Read.mnist('C:/Users/minjk/Downloads/train-labels.idx1-ubyte')
image_test <- Read.mnist('C:/Users/minjk/Downloads/t10k-images.idx3-ubyte')
label_test <- Read.mnist('C:/Users/minjk/Downloads/t10k-labels.idx1-ubyte')

images_train <- matrix(as.numeric(image_train$pic),
                       nrow = nrow(image_train$pic),
                       ncol = ncol(image_train$pic),
                       byrow = TRUE)

labels_train <- label_train$labels

images_test <- matrix(as.numeric(image_test$pic),
                      nrow = nrow(image_test$pic),
                      ncol = ncol(image_test$pic),
                      byrow = TRUE)

labels_test <- label_test$labels

# digits {0,1,2,3,4}

images_train <- images_train[labels_train <= 4, ]
labels_train <- labels_train[labels_train <= 4]

images_test <- images_test[labels_test <= 4, ]
labels_test <- labels_test[labels_test <= 4]

# check dim
dim(images_train) #30596 * 784
dim(images_test)  #5139 * 784

# 28*28 to 14*14 pixel-averaged matrices by taking the mean of 4 neighbour elements
x1 = rep(seq(1,28,2), times = 14) + 28*rep(seq(0,26,2), each = 14)

images_train <- (images_train[, x1] + images_train[, x1+1]+ images_train[, x1+28] +images_train[, x1+29])/4

images_test <- (images_test[, x1] + images_test[, x1+1]+ images_test[, x1+28] +images_test[, x1+29])/4

# check dim
dim(images_train)  #30596 * 196
dim(images_test)   #5139 * 196

# rescale to (0, 1)
View(images_train)
images_train = images_train / 255
images_test = images_test / 255







