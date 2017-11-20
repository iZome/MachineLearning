## Libraries and seed
rm(list = ls())
library(mxnet)
library(caret)
set.seed(420)

#-------------------#

## Data

path_to_here <- getwd()

train_data <- read.csv(paste0(path_to_here, "/data/Train_Digits_20171108.csv"), header = TRUE)
unclassified_data <- read.csv(paste0(path_to_here, "/data/Test_Digits_20171108.csv"), header = TRUE)

#train_data[,1] <- as.factor(train_data[, 1])

# split training set into training and test set

split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)
test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ] 

# convert to matrix, required by "mxnet"

train <- data.matrix(train_data)
test <- data.matrix(test_data)

train_x <- t(train[, -1])
train_y <- train[, 1]

train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

#train_x <- t(train_x)#/255)

test_x <- test[, -1]
test_y <- test[, 1]

test_x <- t(test_x)#/255)


# transpose and normalize to more   

#-------------------#

## Setting up Convolutional Neural Network(CNN)

data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=10)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")

devices <- mx.cpu()

mx.set.seed(0)

model <- mx.model.FeedForward.create(softmax, X=train_array, y=train_y,
                                     ctx=devices, num.round=10, array.batch.size=100,
                                     learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                     initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100))

preds <- predict(model, test)

