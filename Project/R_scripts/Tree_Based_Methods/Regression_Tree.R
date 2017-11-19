## Libraries and seed
library(caret)
library(readr)
library(tree)
library(randomForest)
library(gbm)

set.seed(420)

#-------------------#

## Data

path_to_here <- getwd()

train_data <- read.csv(paste0(path_to_here, "/data/Train_Digits_20171108.csv"))
unclassified_data <- read.csv(paste0(path_to_here, "/data/Test_Digits_20171108.csv"))

# Remove unnessesary varibles which have a low variance
split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)
test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ]

near_zero_variables <- nearZeroVar(train_data[,-1], saveMetrics = T, freqCut = 10000/1,uniqueCut = 1/7)
cut_variables <- rownames(near_zero_variables[near_zero_variables$nzv == TRUE,])
variables <- setdiff(names(train_data), cut_variables)
train_data <- train_data[, variables]
test_data <- test_data[, variables]

train_data_odd_even <- train_data
train_data_odd_even[,1] <- as.factor(train_data_odd_even[,1] %% 2)
train_data[,1] <- as.factor(train_data[,1])

unclassified_data[,1] <- as.factor(unclassified_data[,1])

sum(near_zero_variables$nzv)

#-------------------#

## REGRESSION - tree

regression <- function(
    minimum_development,
    data
    ){
        minimum_development <- 0.005
        tree_model <- tree(Digit ~ ., data = data, mindev = minimum_development)
        plot(tree_model)
        text(tree_model, cex = .5)
        print(summary(tree_model))
    
        cross_validation <- cv.tree(tree_model, K = 10)
        cross_validation$k[1] <- 0
        alpha <- round <- round(cross_validation$k)
        
        plot(cross_validation$size, cross_validation$dev, type = "b", 
             xlab = "Number of terminal nodes", ylab = "CV error")
        
        tree_prune <- prune.tree(tree_model, best = 20)
        summary(tree_prune)
    }

#-------------------#

## RANDOM FORREST -randomForest



#-------------------#

## BOOSTING - gbm

#-------------------#
