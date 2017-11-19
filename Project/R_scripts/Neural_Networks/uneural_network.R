## Libraries and seed
library(h2o)
library(caret)
library(reshape2)

set.seed(420)

#-------------------#

## Data

path_to_here <- getwd()

train_data <- read.csv(paste0(path_to_here, "/data/Train_Digits_20171108.csv"))
unclassified_data <- read.csv(paste0(path_to_here, "/data/Test_Digits_20171108.csv"))

local.h2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads = -1)

train_data[,1] <- as.factor(train_data[, 1])
split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)
test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ]

train_data <- as.h2o(train_data)
unclassified_data <- as.h2o(unclassified_data)
test_data <- as.h2o(test_data)

#-------------------#

## Getting useful data from grid run of neural networkss

get_data_in_df <- function(
    data
)
{
    n <- length(data@model_ids)
    mse_errors <- rep(0,n)
    mean_per_class_errors <- rep(0,n)
    hidden <- rep("", n)
    str(hidden)
    rate <- rep(0,n)
    l1 <- rep(0,n)
    epochs <- rep(0,n)
    model_numbers <- rep(0,n)
    train_error <- rep(0,n)
    train_mse <- rep(0,n)
    test_error <- rep(0,n)
    test_mse <- rep(0,n)
    activation <- rep("",n)
    input_dropout_ratio <- rep(0,n)
    nesterov_accelerated_gradient <- rep("", n)
    
    model_df <- data.frame(model_numbers = mse_errors, hidden, rate, l1, epochs,
                           train_error, test_error, train_mse, test_mse, activation, input_dropout_ratio, stringsAsFactors = FALSE)
    str(model_df)
    
    for(i in 1:n){
        model <- h2o.getModel(data@model_ids[[i]])
        model_df$mse_errors[i] <- h2o.mse(model)
        #model_df$mean_per_class_error[i] <- model@model$cross_validation_metrics@metrics$mean_per_class_error
        model_df$mean_per_class_error[i] <- h2o.performance(model, xval = T)@metrics$mean_per_class_error
        
        model_paramaters <- model@allparameters
        model_name <- model@model_id
        model_number <- sub(".*model_(.*)$", "\\1", model_name)
        model_df$model_numbers[i] <- as.integer(model_number)
        model_df$hidden[i] <- paste(as.character(model_paramaters$hidden), sep = " ", collapse = ", ")
        model_df$rate[i] <- model_paramaters$rate
        model_df$l1[i] <- model_paramaters$l1
        model_df$epochs[i] <- model_paramaters$epochs
        model_df$activation[i] <- model_paramaters$activation
        model_df$input_dropout_ratio[i] <- model_paramaters$input_dropout_ratio
        model_df$nesterov_accelerated_gradient[i] <- model_paramaters$nesterov_accelerated_gradient
        
        #print(model)
        train_performance <- h2o.performance(model, train_data)@metrics
        train_performance_error <- train_performance$mean_per_class_error
        train_performance_mse <- train_performance$MSE
        
        model_df$train_error[i] <- train_performance_error
        model_df$train_mse[i] <- train_performance_mse
        
        test_performance <- h2o.performance(model, test_data)@metrics
        test_predictions <- h2o.predict(model, test_data)
        test_accuracy <- test_predictions$predict == test_data$Digit
        test_performance_error <- 1 - mean(test_accuracy)
        #test_performance_error <- test_performance$mean_per_class_error
        test_performance_mse <- test_performance$MSE
        
        model_df$test_error[i] <- test_performance_error
        model_df$test_mse[i] <- test_performance_mse
        
    }
    model_df <- model_df[with(model_df, order(model_numbers)),]
    model_df
}

activation <- list("Rectifier", "RectifierWithDropOut")# "Tanh")
hidden <- list(c(100,100), c(150, 150), c(100, 100, 100)) #c(100, 100, 100))#, c(150, 150, 150))
input_dropout_ratio <- list(0, 0.2)
nesterov_accelerated_gradient <- list( TRUE, FALSE)
epochs <- list(20)#, 20)
l1 = list(1.4e-5)
hyper_params <- list(activation = activation, hidden = hidden, input_dropout_ratio = input_dropout_ratio, nesterov_accelerated_gradient = nesterov_accelerated_gradient, epochs = epochs, l1 = l1)

grid_deep_learning <- h2o.grid(algorithm = "deeplearning",
                               x = 2:785,
                               y = 1,
                               training_frame = train_data,
                               nfolds = 10,
                               stopping_metric = "MSE",
                               stopping_tolerance = 0.0025,
                               hyper_params = hyper_params)
    save_results <- function(results){
    write.csv(results, file = paste0(path_to_here, "/Neural_Networks/results_NN/grid_run_evenodd2.csv"))
}

df <- get_data_in_df(grid_deep_learning)
save_results(df)

results_df <- df

results_df <- results_df[with(results_df, order(mean_per_class_error)),]
results_df$row_names <- 1:length(results_df[,1])

melt_datas <- melt(results_df[c("test_error","mean_per_class_error", "row_names",
                                "model_numbers")], id = c("row_names", "model_numbers"))

# Plot classification error
plot_list[[1]] <- ggplot(data=melt_datas,
                         aes(x=row_names, y=value)) +
    geom_point(aes(colour = as.factor(model_numbers), group = as.factor(model_numbers)), size = 3) +
    geom_line(aes(group = variable)) +
    labs(y = "Missclassification error in range 0 to 1",
         x = "Models",
         title = "Missclassification error for training model and test set",
         caption = "Top - Training model, Bottom - Test set",
         colour = "Model id") +
    scale_y_continuous(limits = c(0, 0.2))
ggsave(paste0(path_to_here,"/Neural_Networks/results_NN/per_class_error3.png"))

                              deep_learning_predicting <- h2o.predict(object = deep_learning_results, newdata = test_data)
deep_learning_performance <- h2o.performance(model = deep_learning_results3, newdata = test_data)
deep_learning_performance
deep_learning_predicting_data_frame <- as.data.frame((deep_learning_predicting))


deep_learning_results2 <- h2o.deeplearning(x = 2:785,
                                           y = 1,
                                           training_frame = train_data,
                                           activation = "Tanh",
                                           hidden = c(160, 160, 160, 160, 160),
                                           nfolds = 10,
                                           keep_cross_validation_predictions = TRUE,
                                           epochs = 40)

deep_learning_results3<- h2o.deeplearning(x = 2:785,
                                          y = 1,
                                          training_frame = train_data,
                                          #activation = "RectifierWithDropout",
                                          activation = "Rectifier",
                                          input_dropout_ratio = 0.2,
                                          #hidden_dropout_ratios = c(0.2, 0.2, 0.2),
                                          nfolds = 10,
                                          balance_classes = TRUE,
                                          hidden = c(150, 150, 150),
                                          momentum_stable = 0.99,
                                          nesterov_accelerated_gradient = TRUE,
                                          epochs = 15)

h2o.performance(deep_learning_results3, test_data)
