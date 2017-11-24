## Libraries and seed
rm(list = ls())
library(caret)          # useful library to split up data set
library(tikzDevice)     # library to export plots to .tex files
library(gbm)            # library with powerful boosting method
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

train_data <- read.csv(paste0(path_data, "/Train_Digits_20171108.csv"), header = TRUE)
unclassified_data <- read.csv(paste0(path_data, "/Test_Digits_20171108.csv"), header = TRUE)

train_data[,1] <- as.factor(train_data[, 1])

# split training set into training and test set

split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)
test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ] 

#-------------------#

## Boosting
# Train booster
boosting <- function(
    data,
    n_trees,
    minimum_development = 0.01,
    interaction_depth = 2,
    shrinkage = 0.001
){
    # boosting <- gbm(Digit ~ .,
    #                 data = data,
    #                 distribution = "multinomial",
    #                 n.trees = n_trees,
    #                 interaction.depth = interaction_depth,
    #                 shrinkage = shrinkage,
    #                 
    #                 bag.fraction = 1,
    #                 cv.folds = 10,
    #                 n.cores = 4)
    
    
    tune_control <- trainControl(method = "cv",
                                 number = 5,
                                 repeats = 1)
    training_grid <- expand.grid(n.trees = c(n_trees), 
                                 interaction.depth = c(interaction_depth),
                                 shrinkage = c(shrinkage),
                                 n.minobsinnode = c(10))
    print(training_grid)
    boosting <- train(Digit ~ ., data = data, method = "gbm",
                      trControl = tune_control,
                      tuneGrid = training_grid)
    return(boosting)
}


# Plot error as the number of trees increase

plot_error_development <- function(
    boosting_data,
    destination_path
){
    error_data <- data.frame(n_trees = 1:length(boosting_data$cv.error), 
                             error <- boosting_data$cv.error)
    write.csv(error_data, file = paste0(destination_path ,".csv"))
    
    ggplot1 <- ggplot(data = error_data, aes(x = n_trees)) +
        geom_line(aes(y = error, colour = "$Boosting$")) +
        xlab("$n_{trees}$") +
        ylab("Miss.class. Error") +
        scale_colour_manual("Legend",
                            breaks = c("$Boosting$"),
                            values = c("black"),
                            guide = guide_legend(override.aes = list(
                                linetype = c("solid"),
                                shape = c( 16)
                            ))) +
        theme(legend.position = c(0.9, 0.2)) +
        theme_bw() +
        theme(legend.position = c(0.8, 0.355),
              legend.background = element_rect(fill=alpha('white', 0)))
    ggsave(paste0(destination_path, ".png"))
    
    ggplot_to_latex(ggplot1, destination_path, width = 6, height = 4)
}

main <- function(){
    n_trees = 10
    boosting_train <- boosting(train_data,n_trees)
    plot_error_development(boosting_train, paste0(path_to_here, 
                                                  "/Results_TBM/Boosting_",
                                                  n_trees,
                                                  "trees_Error_plot"))
    #predicted <- predict(boosting_train, test_data)
    #create_confusion_matrix(predicted, test_data$Digit, paste0(path_to_here, 
    #                                                           "/Results_TBM/Boosting_",
    #                                                           n_trees))
}

main()

