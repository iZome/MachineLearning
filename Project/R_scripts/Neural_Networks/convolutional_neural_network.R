## Libraries and seed
rm(list = ls())
library(mxnet)
library(caret)

set.seed(420)

# Load help script with functions to export the results to latex
# These functions gathered to avoid duplicate code
if(!exists("create_confusion_matrix", mode = "function")){
    source("Help_Scripts/to_latex_functions.R")
}
#-------------------#

## Data

path_data <- getwd()
path_to_here <- paste0(getwd(), "/Neural_Networks")

train_data <- read.csv(paste0(path_data, "/data/Train_Digits_20171108.csv"))
unclassified_data <- read.csv(paste0(path_data, "/data/Test_Digits_20171108.csv"))

# Split the data into training, test and validation set
split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)

test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ]

split_train_validation <- createDataPartition(train_data$Digit, p = 0.85, list = FALSE)

validation_data <- train_data[-split_train_validation, ]
train_data <- train_data[split_train_validation, ]

# Setting up datasets as matrices in order to get correct input for mxnet

train <- as.matrix(train_data)
train_x <- t(train[, -1])
train_y <- train[, 1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_x <- t(test_data[, -1])
test_y <- test_data[, 1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))

validation_x <- t(validation_data[, -1])
validation_y <- validation_data[, 1]
validation_array <- validation_x
dim(validation_array) <- c(28, 28, 1, ncol(validation_x))

#-------------------#

data <- mx.symbol.Variable("data")

# Setting up first convolutional layer

convolution_1 <- mx.symbol.Convolution(data = data, kernel = c(5,5), num_filter = 40)
activation_1 <- mx.symbol.Activation(data = convolution_1, act_type = "tanh")
pooling_1 <- mx.symbol.Pooling(data = activation_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2))

# Setting up second convolutional layer

convolution_2 <- mx.symbol.Convolution(data = pooling_1, kernel = c(5,5), num_filter = 80)
activation_2 <- mx.symbol.Activation(data = convolution_2, act_type = "tanh")
pooling_2 <- mx.symbol.Pooling(data = activation_2, pool_type = "max", kernel = c(2, 2), stride = c(2,2))

# Setting up first fully connected layer

flatten <- mx.symbol.Flatten(data = pooling_2)
fully_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
activation_3 <- mx.symbol.Activation(data = fully_1, act_type = "tanh")

# Setting up second fully connected layer

fully_2 <- mx.symbol.FullyConnected(data = activation_3, num_hidden = 40)

# Output layer, softmax gives probabilies for the output

neural_net_model <- mx.symbol.SoftmaxOutput(data = fully_2)

mx.set.seed(58)

cpu_used <- mx.cpu()

train_model <- mx.model.FeedForward.create(neural_net_model,
                                           X = train_array,
                                           y = train_y,
                                           eval.data = list(data = validation_array, label = validation_y),
                                           ctx = cpu_used,
                                           num.round = 400,
                                           array.batch.size = 50,
                                           learning.rate = 0.01,
                                           momentum = 0.9,
                                           eval.metric = mx.metric.accuracy,
                                           epoch.end.callback = mx.callback.log.train.metric(100)
                                           )