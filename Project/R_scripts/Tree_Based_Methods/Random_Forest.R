## Libraries and seed
rm(list = ls())
library(randomForest)   # library giving a easy-to-use random forest method
library(caret)          # useful library to split up data set
library(tikzDevice)     # library to export plots to .tex files
set.seed(420)           # seed to replicate results and get consistent test and training set

options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}", "\\usepackage[T1]{fontenc}", 
                               "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))

#-------------------#

## Data

path_to_here <- getwd()

train_data <- read.csv(paste0(path_to_here, "/data/Train_Digits_20171108.csv"), header = TRUE)
unclassified_data <- read.csv(paste0(path_to_here, "/data/Test_Digits_20171108.csv"), header = TRUE)

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
        print(error_data)
        tikz(file = paste0(destination_path, ".tex"), width = 6, height = 4)
        ggplot1 <- ggplot(data = error_data, aes(x = n_trees)) +
            geom_line(aes(y = error, colour = "$Random forest$")) +
            xlab("n_{trees}") +
            ylab("Miss.class. Error") +
            scale_colour_manual("Legend",
                                breaks = c("$Random forest$"),
                                values = c("black"),
                                guide = guide_legend(override.aes = list(
                                    linetype = c("solid"),
                                    shape = c( 16)
                                ))) +
            theme(legend.position = c(0.9, 0.2))
        ggsave(paste0(destination_path, ".png"))
        print(ggplot1)
        dev.off()
    }
random_forest <- train_random_forest(train_data, 100)
plot_error_development(random_forest, paste0(path_to_here, "/Tree_Based_Methods/Results_TBM/random_forest"))

