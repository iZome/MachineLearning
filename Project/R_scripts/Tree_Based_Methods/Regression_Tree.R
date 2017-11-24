## Libraries and seed
rm(list = ls())
library(caret)
library(readr)
library(tree)
library(randomForest)
library(gbm)
library(tikzDevice)     # library to export plots to .tex files

options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}", "\\usepackage[T1]{fontenc}",
                               "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))

set.seed(420)

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

# Remove unnessesary varibles which have a low variance
split_train_test <- createDataPartition(train_data$Digit, p = 0.8, list = FALSE)
test_data <- train_data[-split_train_test, ]
train_data <- train_data[split_train_test, ]

# Remove variable with low variance which are near zero. Doing it after 
# splitting in train/test set to avoid contaminating the data.
near_zero_variables <- nearZeroVar(train_data[,-1], saveMetrics = T, freqCut = 10000/1,uniqueCut = 1/7)
cut_variables <- rownames(near_zero_variables[near_zero_variables$nzv == TRUE,])
variables <- setdiff(names(train_data), cut_variables)
train_data <- train_data[, variables]
test_data <- test_data[, variables]

train_data[, 1] <- as.factor(train_data[, 1])
test_data[, 1] <- as.factor(test_data[, 1])

#train_data[,1] <- as.factor(train_data[,1])

unclassified_data[,1] <- as.factor(unclassified_data[,1])

sum(near_zero_variables$nzv)

#-------------------#

## REGRESSION - tree

regression <- function(
    minimum_development,
    train_data,
    test_data
    ){
    # Chanage name of pixel columns to work with tikz library
    
    print(getClass(class(train_data)))
    colnames(train_data)[ 2:length(train_data[1,])] <- c(paste0("Pixel", 1:(length(train_data[1,]) - 1)))
    colnames(test_data)[ 2:length(train_data[1, ])] <- c(paste0("Pixel", 1:(length(train_data[1,]) - 1)))
    minimum_development <- 0.005    
    tree_model <- tree(Digit ~ ., data = train_data, mindev = minimum_development)
    plot(tree_model)
    text(tree_model, cex = .5)
    print(summary(tree_model))

    cross_validation <- cv.tree(tree_model, K = 10)
    cross_validation$k[1] <- 0
    alpha <- round <- round(cross_validation$k)
    
    plot(cross_validation$size, cross_validation$dev, type = "b", 
         xlab = "Number of terminal nodes", ylab = "CV error")
    
    ggplot_df <- data.frame(size = cross_validation$size, dev = cross_validation$dev)
    
    destination_path <- paste0(path_to_here, "/Results_TBM/Regression_Tree")
    
    ggplot1 <- ggplot(data = ggplot_df, aes(x = size, y = dev)) +
               geom_line(aes(colour = "$RegressionTree$"), linetype = "dashed") +
               geom_point() +
               geom_vline(xintercept = 20, color = "black", linetype = "dotdash") +
               xlab("n\\_{terminal nodes}") +
               ylab("CV Error") +
               scale_colour_manual("Legend",
                                   breaks = c("$RegressionTree$"),
                                   values = c("#91bfdb"),
                                   guide = guide_legend(override.aes = list(
                                       linetype = c("solid"),
                                       shape = c(16)
                                   ))) +
               theme_bw() +
               theme(legend.position = c(0.8, 0.355),
                  legend.background = element_rect(fill=alpha('white', 0)))
    ggsave(paste0(destination_path, ".png"))
    
    ggplot_to_latex(ggplot1, destination_path, width = 5, height = 5)
    tree_prune <- prune.tree(tree_model, best = 20)
    summary(tree_prune)
    
    tikz(file = paste0(destination_path, "_Tree.tex"), width = 6, height = 4)
    plot(tree_prune)
    text(tree_prune, cex = .5)
    dev.off()
    
    predicted <- predict(tree_prune, test_data, type = "class")
    create_confusion_matrix(predicted, test_data[,1], destination_path)
    }

regression(0.05, train_data, test_data)
#-------------------#

## RANDOM FORREST -randomForest



#-------------------#

## BOOSTING - gbm

#-------------------#
