rm(list=ls())
library(mxnet)
library(caret)

set.seed(69)

input <- read.csv("data/Train_Digits_20171108.csv")
input$Digit <- input$Digit%%2
input$Digit <- as.factor(input$Digit)

rotate <- function(x) t(apply(x, 2, rev))
trans <- rotate(rotate(input[2:ncol(input)]))
t <- cbind(input$Digit, trans)


#Split to training and test set
sample <- sample(nrow(input)*0.8)
train <- input[sample,]
test <- input[-sample,]

# Set up train and test datasets
train <- data.matrix(train)
train_x <- t(train[, -1])
train_y <- train[, 1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

test_x <- t(test[, -1])
test_y <- test[, 1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))



data <- mx.symbol.Variable('data')
# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_2)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")
# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 40)
# Output. Softmax output since we'd like to get some probabilities.
NN_model <- mx.symbol.SoftmaxOutput(data = fc_2)

# Set seed for reproducibility
mx.set.seed(100)

# Device used. CPU in my case.
devices <- mx.cpu()

# Train the model
model <- mx.model.FeedForward.create(NN_model,
                                     X = train_array,
                                     y = train_y,
                                     ctx = devices,
                                     num.round = 5,
                                     array.batch.size = 50,
                                     learning.rate = 0.01,
                                     momentum = 0.9,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))

# Predict labels
predicted <- predict(model, test_array)

# Assign labels
predicted_labels <- max.col(t(predicted)) - 2

table(test$Digit, predicted_labels)

# Get accuracy
sum(diag(table(test[, 1], predicted_labels)))/500


makeConfusionMatrix <- function(pred, true){
    mod <- confusionMatrix(pred,true)
    confM <- mod$table
    confM <- rbind(confM, Totals = rowSums(confM))
    sums <- rowSums(confM)
    confM <- cbind(confM, Error = 0)
    for (row in 1:(nrow(confM)-1)){
        correct <- confM[row,row]
        wrong <- sums[row] - confM[row,row]
        confM[row,"Error"] = round(wrong/(correct+wrong),5)
    }
    confM[nrow(confM), "Error"] <- 1 - mod$overall["Accuracy"]
    return(confM)
}
