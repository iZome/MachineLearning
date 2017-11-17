## LIBRARIES ##
library(plot3D)
library(colorRamps)
library(h2o)
library(xtable)
set.seed(1234)
## LIBRARIES ##


## FILE PATH ##
current_directory <- getwd()
train_path <- paste0(current_directory, "/data/fashion-mnist_train.csv")
test_path <- paste0(current_directory, "/data/fashion-mnist_test.csv")

# Read train and test sets
train_df <- read.csv(train_path)
test_df <- read.csv(test_path)

# Select random catgories
random_categories <- sample.int(10,4) - 1 # sample numbers between 0 and 9
random_categories <- c(2,3,4,8) # Categories used in run presented in report

# Filter out categories selected
train_df <- train_df[train_df$label %in% random_categories,]
test_df <- test_df[test_df$label %in% random_categories,]
print(dim(train_df))
print(dim(test_df))

## FILE PATH ##

## INITIALIZE H2O ##
localH2O <- h2o.init(ip = "localhost", port = 54321, nthreads = -1  , max_mem_size = "2G")

# Load train and test set to H2O
train <- as.h2o(train_df, destination_frame = "train")
test <- as.h2o(test_df, destination_frame = "test")

# Factor label to make confusion matrix in results
train$label <- as.factor(train$label)
test$label <- as.factor(test$label)

## INITIALIZE H2O ##

## METHODS ##

# Run grid with 16 combinations of parameters for a neural network
run_neural_network <- function()
    {

        hidden = list(c(16,16), c(128,128), c(256,256), c(128,128,128))
        l1 = list(1.4e-5)
        rate = list(0.1,0.01)

        epochs = list(10,20)
        hyper_params = list(hidden = hidden, l1 = l1, epochs = epochs, rate = rate)
        model2 <- h2o.grid(algorithm = "deeplearning",
                          x = 2:785,
                          y = 1,
                          training_frame = train,
                          nfolds = 10,
                          stopping_metric = "MSE",
                          stopping_tolerance = 0.00025,
                          #validation_frame = test,
                          hyper_params = hyper_params
                          )
        # Show a short summary of the best model from the grid
        model1 <- h2o.getModel(model2@model_ids[[1]])
        h2o.mse(model1);
        summary(model1)

        # Save best model to folder "data", make sure this is created
        saveH2O(model1, paste0(current_directory, "/data"))
        model2
    }


## Save functions
# H2O have not yet got a save grid method of it's own, so manually saves each
# model in the grid to a directory
save_grid <- function(
    grid,
    directory
    )
    {
    model_ids <- grid@model_ids
    mse_errors <- rep(0,length(model_ids))
    for(i in 1:length(model_ids)){
        model <- h2o.getModel(grid@model_ids[[i]])
        saveH2O(model,directory)
    }
}

# Save single model to directory
saveH2O <- function(
    model,
    directory
    )
    {
        h2o.saveModel(model, path = directory, force = T)
    }

# Save data frame to file
saveDataFrame <- function(
    df,
    directory,
    name
    )
    {
        write.csv(df, paste0(directory, name))
    }

## Load functions
# Load H2O model
loadH2O <- function(
    path
    )
    {
        h2o.loadModel(path)
    }

# Load data frame
loadDataFrame <- function(
    path
    )
    {
        read.csv(path)
    }

# Load multiple H2O models
load_results <- function(
    path
    )
    {
        names <- list.files(path = path)
        names <- paste0(path, names)
        myfiles <- lapply(names, loadH2O)
        myfiles
    }


make_list <- function(
    grid
    )
    {
        model_ids <- grid@model_ids
        n_ids <- length(model_ids)
        grid_list <- list()
        for(i in 1:n_ids){
          grid_list <- c(grid_list, h2o.getModel(nGrid@model_ids[[i]]))
        }
        grid_list
    }


## QUIT H2O ##
quitH2O <- function()
    {
        if(interactive()){
            word <- readline(prompt="Quit H2O(enter \"yes\" or \"no\": ")
            print(word)
        }
        else{
            cat("Quit H2O(enter \"yes\" or \"no\": ")
            word <- readLines("stdin",1)
            cat(word)
        }
        if(word %in% c("y","yes","please do")){
            print("Quiting H2O.")
            h2o.shutdown(prompt = F)
        }
    }
## QUIT H2O ##

## FILTER DATA
get_data_in_df <- function(
    data
    )
    {
        n <- length(data)
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

        model_df <- data.frame(model_numbers = mse_errors, hidden, rate, l1, epochs,
                               train_error, test_error, train_mse, test_mse,stringsAsFactors = FALSE)
        str(model_df)

        for(i in 1:n){
            model <- data[[i]]
            model_df$mse_errors[i] <- h2o.mse(model)
            model_df$mean_per_class_error[i] <- model@model$training_metrics@metrics$mean_per_class_error

            model_paramaters <- model@allparameters
            model_name <- model@model_id
            model_number <- sub(".*model_(.*)$", "\\1", model_name)
            model_df$model_numbers[i] <- as.integer(model_number)
            model_df$hidden[i] <- paste(as.character(model_paramaters$hidden), sep = " ", collapse = ", ")
            model_df$rate[i] <- model_paramaters$rate
            model_df$l1[i] <- model_paramaters$l1
            model_df$epochs[i] <- model_paramaters$epochs

            train_performance <- h2o.performance(model, train)@metrics
            train_performance_error <- train_performance$mean_per_class_error
            train_performance_mse <- train_performance$MSE

            model_df$train_error[i] <- train_performance_error
            model_df$train_mse[i] <- train_performance_mse

            test_performance <- h2o.performance(model, test)@metrics
            test_performance_error <- test_performance$mean_per_class_error
            test_performance_mse <- test_performance$MSE

            model_df$test_error[i] <- test_performance_error
            model_df$test_mse[i] <- test_performance_mse

        }
        model_df <- model_df[with(model_df, order(model_numbers)),]
        model_df
}

# Make confusion matrix of h2o model
best_confusion_matrix_to_latex <- function(
    best_model
    )
    {
        result_best_model <- h2o.performance(best_model, test)
        confusion_table <- result_best_model@metrics$cm$table
        print(xtable(confusion_table, display = c("d","d", "d", "d", "d", "f", "s"),
                     digits = c(0, 0, 0, 0, 3, 4, 0)),
              file = "result_data_frames/confusion_table.tex",
              include.rownames = FALSE )
    }

## FILTER DATA

 #! ALL PLOT FUNCTIONS IN SEPERATE SCRIPT
 # reason is that they are independent of H2O and it being less computationally
 # for the computer and faster running the plot functions in seperate scripts

 ## MAIN ##
main <- function(word)
   {
       print(word)
       #Run NN code and save result models from grid to file, make sure to have a folder called "models"
       # and have the current directory set to the same as one for this R-script if using "Rstudio"
       # , this can be done at the top bar: "Session -> Set working directory -> To source file location"
       if(word  == "Run"){
           nGrid <- run_neural_network()
           save_grid(nGrid, paste0(current_directory, "/models"))

           nGrid <- make_list(nGrid)
           data_df <- get_data_in_df(nGrid)
           saveDataFrame(data_df, paste0(current_directory,"/result_data_frames/"), "16_permutation_run")
       }
       #Load previously run models since a run takes alot of time and finding the data wanted from the models
       else if(word == "Load"){
           loaded <- load_results(paste0(current_directory,"/models/"))
           data_df <- get_data_in_df(loaded)
           print(data_df) # print to get a short overview of the fitered data
           saveDataFrame(data_df, paste0(current_directory,"/result_data_frames/"), "16_permutation_run")
       }
       quitH2O()
  }
## FILTER DATA

#Starts the main function with input from user
#Can either run load or run a new network
#Since there is a difference between interactive and non-interactive mode is
#possible, the code checks which type is used in the current run
if(!interactive()){
   cat("\"Load\" or \"Run\": ")
   word <- readLines("stdin",1)
   #word <- word[1]
   cat(word)
   main(word)
}else{
   word <- readline(prompt="\"Load\" or \"Run\": ")
   print(word)
   main(word)
}

## MAIN ##
