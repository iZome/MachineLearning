## Libraries and seed
rm(list = ls())
library(randomForest)   # library giving a easy-to-use random forest method
library(caret)          # useful library to split up data set
library(tikzDevice)     # library to export plots to .tex files
library(xtable)         # library to export data frames to tables in .tex files
set.seed(420)           # seed to replicate results and get consistent test and training set

options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}", "\\usepackage[T1]{fontenc}", 
                               "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))

#-------------------#

## Data

path_to_here <- paste0(getwd(), "/Tree_Based_Methods")   # getwd give path to project
                                                        # which is one folder over

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
            xlab("n\\_{trees}") +
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
random_forest <- train_random_forest(train_data, 500)
str(random_forest)
plot_error_development(random_forest, paste0(path_to_here, "/Tree_Based_Methods/Results_TBM/random_forest"))

prediction <- predict(random_forest, newdata = test_data)
prediction

create_confusion_matrix <- function(
    predicted_value,
    true_value
){
    conf <- confusionMatrix(predicted_value, true_value)
    conf_df <- as.data.frame.matrix(conf$table) # extract confusion matrix
    # add row for total error
    conf_df <- rbind(conf_df, Total = rep(0, ncol(conf_df)))
    
    rows_in_df <- nrow(conf_df)
    
    classification_frac <- rep("", rows_in_df)
    classification_float <- rep(0, rows_in_df)
    
    total_wrong <- 0
    total_classified <- 0
    
    # make columns that shows accuracy
    for(i in 1:(rows_in_df - 1)){
        correct_classified <- conf_df[i, i]
        amount_classified <- sum(conf_df[i, ])
        missclassified <- amount_classified - correct_classified
        
        classification_frac[i] <- paste0(missclassified, "/", amount_classified)
        classification_float[i] <- missclassified / amount_classified
        
        total_wrong <- total_wrong + missclassified
        total_classified <- total_classified + amount_classified
    }
    
    classification_frac[rows_in_df] <- paste0(total_wrong, "/", total_classified)
    classification_float[rows_in_df] <- total_wrong / total_classified
    
    conf_df <- cbind(temp = row.names(conf_df),           # added extra column 
                     conf_df,                             # to get predicted classes
                     Error = classification_float,
                     Rate = classification_frac)
    names(conf_df) <- c("", names(conf_df)[-1]) # remove name of predicted classes
    
    write.csv(x = conf_df, file = paste0(path_to_here, "/Results_TBM/Random_Forest_Confusion_Matrix.csv"))
    print(xtable(conf_df, display = c("s", rep("d", 11), "f", "s"),
                 digits = c(rep(0, 12), 3, 0)),
          file = paste0(path_to_here, "/Results_TBM/Random_Forest_Confusion_Matrix.tex"),
          include.rownames = FALSE)
                          
    }