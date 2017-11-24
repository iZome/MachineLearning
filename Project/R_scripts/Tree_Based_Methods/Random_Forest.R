## Libraries and seed
rm(list = ls())
library(randomForest)   # library giving a easy-to-use random forest method
library(caret)          # useful library to split up data set
library(tikzDevice)     # library to export plots to .tex files
library(xtable)         # library to export data frames to tables in .tex files
set.seed(420)           # seed to replicate results and get consistent test and training set

# Load help script with functions to export the results to latex
# These functions gathered to avoid duplicate code
if(!exists("create_confusion_matrix", mode = "function")){
    source("Help_Scripts/to_latex_functions.R")
}

#-------------------#

## Data
path_data <- paste0(getwd(), "/data")
path_to_here <- paste0(getwd(), "/Tree_Based_Methods")   # getwd give path to project
                                                        # which is one folder over

train_data <- read.csv(paste0(path_data, "/Train_Digits_20171108.csv"), header = TRUE)
unclassified_data <- read.csv(paste0(path_data, "/Test_Digits_20171108.csv"), header = TRUE)

train_data[,1] <- as.factor(train_data[, 1])

# split training set into training and test set

split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)
test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ]

#-------------------#

## Random forest
# Train forest
train_random_forest <- function(
    data,
    n_trees,
    minimum_development = 0.01
    ){
        random_forest <- randomForest(Digit ~ .,
                                      data = data,
                                      ntree = n_trees,
                                      #mindev = minimum_development,
                                      importance = TRUE,
                                      na.action = na.exclude)
        return(random_forest)
    }

# Plot error as the number of trees increase

plot_error_development <- function(
    random_forest_data,
    destination_path
    ){
        error_data <- data.frame(n_trees = 1:nrow(random_forest_data$err.rate),
                                 error <- random_forest_data$err.rate[,"OOB"])

        write.csv(error_data, file = paste0(destination_path, ".csv"))

        ggplot1 <- ggplot(data = error_data, aes(x = n_trees)) +
            geom_line(aes(y = error, colour = "$Random forest")) +
            xlab("$n\\_{trees}$") +
            ylab("Miss. class. Error") +
            scale_colour_manual("Legend",
                                breaks = c("$Random forest$"),
                                values = c("black"),
                                guide = guide_legend(override.aes = list(
                                    linetype = c("solid"),
                                    shape = c( 16)
                                ))) +
            theme(legend.position = c(0.9, 0.2))
        ggsave(paste0(destination_path, ".png"))

        ggplot_to_latex(ggplot1, destination_path, width = 6, height = 4)
}

main <- function(){
    n_trees = 50
    random_forest <- train_random_forest(train_data, n_trees)
    plot_error_development(random_forest, paste0(path_to_here, "/Results_TBM/Random_Forest_",
                                                 n_trees, "trees_Error_plot"))

    prediction <- predict(random_forest, newdata = test_data)
    create_confusion_matrix(predicted_value = prediction, true_value = test_data$Digit,
                            paste0(path_to_here, "/Results_TBM/Random_Forest_",
                                                                n_trees, "trees"))
}

main()
