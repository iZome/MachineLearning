## Libraries and seed
rm(list = ls())
library(mxnet)      #library for running convolutional neural network
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

train_data[, 1] <- as.factor(train_data[, 1])
train_data_full <- train_data

# Split the data into training, test and validation set
split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)

test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ]

split_train_validation <- createDataPartition(train_data$Digit, p = 0.85, list = FALSE)

validation_data <- train_data[-split_train_validation, ]
train_data_val <- train_data[split_train_validation, ]

# Setting up datasets as matrices in order to get correct input for mxnet

# Training after tuning
train <- train_data
train_x <- t(train[, -1])
train_y <- train[, 1]
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

# Training under tuning
train_val <- train_data_val
train_x_val <- t(train_val[, -1])
train_y_val <- train_val[, 1]
train_array_val <- train_x_val
dim(train_array_val) <- c(28, 28, 1, ncol(train_x_val))

# Test set to compare with other methods
test_x <- t(test_data[, -1])
test_y <- test_data[, 1]
test_array <- test_x
dim(test_array) <- c(28, 28, 1, ncol(test_x))

# Validation set used to validate under tuning
validation_x <- t(validation_data[, -1])
validation_y <- validation_data[, 1]
validation_array <- validation_x
dim(validation_array) <- c(28, 28, 1, ncol(validation_x))

# Get right format on the unclassified set
unclassified_x <- t(unclassified_data[, -1])
unclassified_array <- unclassified_x
dim(unclassified_array) <- c(28, 28, 1, ncol(unclassified_x))

# Full training data when making prediction for unclassified data
train_full <- train_data_full
train_x_full <- t(train_full[, -1])
train_y_full <- train_full[, 1]
train_array_full <- train_x_full
dim(train_array_full) <- c(28, 28, 1, ncol(train_x_full))

#-------------------#

data <- mx.symbol.Variable("data")

two_layer_concolutional_network <- function(){
    # Setting up first convolutional layer
    
    convolution_1 <- mx.symbol.Convolution(data = data, kernel = c(5,5), num_filter = 30)
    activation_1 <- mx.symbol.Activation(data = convolution_1, act_type = "tanh")
    pooling_1 <- mx.symbol.Pooling(data = activation_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2))
    
    # Setting up second convolutional layer
    
    convolution_2 <- mx.symbol.Convolution(data = pooling_1, kernel = c(5,5), num_filter = 50)
    activation_2 <- mx.symbol.Activation(data = convolution_2, act_type = "tanh")
    pooling_2 <- mx.symbol.Pooling(data = activation_2, pool_type = "max", kernel = c(2, 2), stride = c(2,2))
    
    # Setting up first fully connected layer
    
    flatten <- mx.symbol.Flatten(data = pooling_2)
    fully_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
    activation_3 <- mx.symbol.Activation(data = fully_1, act_type = "tanh")
    
    # Setting up second fully connected layer
    
    fully_2 <- mx.symbol.FullyConnected(data = activation_3, num_hidden = 10)
    
    return(fully_2)
    
}

# three layer convolutional neural network to experiment, turned out to perform worse
three_layer_concolutional_network <- function(){
    # Setting up first convolutional layers
    
    convolution_1 <- mx.symbol.Convolution(data = data, kernel = c(5,5), num_filter = 30)
    activation_1 <- mx.symbol.Activation(data = convolution_1, act_type = "tanh")
    pooling_1 <- mx.symbol.Pooling(data = activation_1, pool_type = "max", kernel = c(2, 2), stride = c(2,2))
    
    # Setting up second convolutional layers
    
    convolution_2 <- mx.symbol.Convolution(data = pooling_1, kernel = c(5,5), num_filter = 50)
    activation_2 <- mx.symbol.Activation(data = convolution_2, act_type = "tanh")
    pooling_2 <- mx.symbol.Pooling(data = activation_2, pool_type = "max", kernel = c(2, 2), stride = c(2,2))
    
    # Setting up thrid convolutional layers
    convolution_3 <- mx.symbol.Convolution(data = pooling_2, kernel = c(3,3), num_filter = 50)
    activation_3 <- mx.symbol.Activation(data = convolution_3, act_type = "tanh")
    pooling_3 <- mx.symbol.Pooling(data = activation_3, pool_type = "max", kernel = c(2, 2), stride = c(2,2))
    
    # Setting up first fully connected layer
    
    flatten <- mx.symbol.Flatten(data = pooling_3)
    fully_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
    activation_3 <- mx.symbol.Activation(data = fully_1, act_type = "tanh")
    
    # Setting up second fully connected layer
    
    fully_3 <- mx.symbol.FullyConnected(data = activation_3, num_hidden = 40)
    
    return(fully_3)
    
}

# Output layer, softmax gives probabilies for the output

run_convolutional_neural_network <- function(
    neural_net_model,
    train_array,
    train_y,
    test_array,
    validation_array = NULL,
    validation_y = NULL
){
    mx.set.seed(100)
    
    cpu_used <- mx.cpu()
    
    train_y <- as.factor(as.integer(train_y) %% 2)
    validation_y <- as.factor(as.integer(validation_y) %% 2)
    
    logger_mx <- mx.metric.logger$new()
    train_model <- mx.model.FeedForward.create(neural_net_model,
                                               X = train_array,
                                               y = train_y,
                                               eval.data = list(data = validation_array, label = validation_y), #must be set when validating
                                               ctx = cpu_used,
                                               num.round = 40,
                                               array.batch.size = 50,
                                               learning.rate = 0.01,
                                               momentum = 0.9,
                                               initializer = mx.init.Xavier(),
                                               eval.metric = mx.metric.accuracy,
                                               epoch.end.callback = mx.callback.log.train.metric(100, logger_mx)
                                               )
    
    predicted <- predict(train_model, test_array)
    #mxnet gives wrong labels by 1, that is 1-10 instead of 0-9
    #fix by subtracting 2
    predicted_labels <- max.col(t(predicted)) - 2
    
    return(list(logger_mx, predicted_labels))
}

plot_error_development <- function(
    neural_net_model,
    train_array_val,
    train_y_val,
    test_array,
    test_y,
    validation_array,
    validation_y
){
    # NB! remember to remove "#" from "eval.data" in "run_convolutional_neural_network"
    # run cnn to find error in and val
    error <- run_convolutional_neural_network(neural_net_model,
                                              train_array_val,
                                              train_y_val,
                                              test_array,
                                              validation_array = validation_array,
                                              validation_y = validation_y)
    
    # extract error from results
    error_t <- error[[1]]
    
    error_in_sample <- (1 - error_t$train)
    error_val <- (1 - error_t$eval)
    
    ggplot_df <- data.frame(round = 1:length(error_in_sample),
                            error_in_sample = error_in_sample,
                            error_val = error_val)
    
    # plot error in and validation
    ggplot1 <- ggplot(data = ggplot_df, aes(x = round)) +
        geom_line(aes(y = error_in_sample, colour = "$E_{in}$")) +
        geom_line(aes(y = error_val, colour = "$E_{val}$")) +
        xlab("$n_{round}$") +
        ylab("Miss.class. Error") +
        scale_y_continuous(limits = c(0, 1.0)) +
        scale_colour_manual("Legend",
                            breaks = c("$E_{in}$", "$E_{val}$"),
                            values = c("#91bfdb", "#fc8d59"),
                            guide = guide_legend(override.aes = list(
                                linetype = c("solid", "solid"),
                                shape = c( 16, 16)
                            ))) +
        theme_bw() +
        theme(legend.position = c(0.8, 0.355),
              legend.background = element_rect(fill=alpha('white', 0)))
    ggplot_to_latex(ggplot1, paste0(path_to_here, "/results_NN/98_attempt_convolutional_neural_network_40_rounds"),
                    width = 6, height = 4)
}

# Predict on test set splitted from the training data to compare with other methods
predict_on_test_set <- function(
    neural_net_model,
    train_array,
    train_y,
    test_array,
    test_y
){
    predicted <- run_convolutional_neural_network(neural_net_model,
                                                  train_array,
                                                  train_y,
                                                  test_array)
    predicted <- predicted[[2]]
    
    create_confusion_matrix(as.factor(predicted), test_y,
                            paste0(path_to_here, "/results_NN/convolutional_neural_network_40_rounds_test"))
}

# Give predictions on unclassified data
predict_on_unclassified_data <- function(
    neural_net_model,
    train_array_full,
    train_y_full,
    unclassified_array
){
    predicted <- run_convolutional_neural_network(neural_net_model,
                                                  train_array_full,
                                                  train_y_full,
                                                  unclassified_array)
    predicted <- predicted[[2]]
    
    predicted_digit <- predicted
    predicted_odd_even <- as.factor(as.integer(predicted) %% 2)
    prediction_csv <- data.frame(number = 1:length(predicted_digit),
                                digits = predicted_digit,
                                odd_even = predicted_odd_even)
    write.csv(prediction_csv, file = paste0(path_data, "/data/predictions_STNKAR012.csv"))
    
}

main <- function()
    {
    # Validation used under tuning, set to false so it does run after tuning
    validation_boolean <- TRUE
    # Test used under testing of the final tuned model to compare with other methods
    train_boolean <- FALSE
    
    fully_2 <- two_layer_concolutional_network()
    neural_net_model <- mx.symbol.SoftmaxOutput(data = fully_2)
    
    if(validation_boolean){
        plot_error_development(neural_net_model,
                               train_array_val,
                               train_y_val,
                               test_array,
                               test_y,
                               validation_array,
                               validation_y)
    }
    
    if(train_boolean){
        predict_on_test_set(neural_net_model,
                            train_array,
                            train_y,
                            test_array,
                            test_y)
    }
    
    predict_on_unclassified_data(neural_net_model,
                                 train_array_full,
                                 train_y_full,
                                 unclassified_array)
    
}
main()


