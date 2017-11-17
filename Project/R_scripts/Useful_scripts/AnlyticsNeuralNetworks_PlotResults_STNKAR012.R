library(colorRamps)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(xtable)

# Please make sure all libraries are available before running

current_directory <- getwd()

# Load result data frame
results_df <- read.csv(paste0(current_directory,
                              "/result_data_frames/16_permutation_run"))

results_df <- results_df[,-1]
results_df <- results_df[with(results_df, order(test_error)),]
results_df$row_names <- as.numeric(1:nrow(results_df))
results_df
plot_list <- list()

melt_datas <- melt(results_df[c("test_error","mean_per_class_error", "row_names",
                   "model_numbers")], id = c("row_names", "model_numbers"))

melt_check <- melt(results_df[c("mse_errors", "mean_per_class_error", "row_names",
                   "model_numbers")], id = c("row_names", "model_numbers"))
melt_datas_mse <- melt(results_df[c("test_mse","mse_errors", "row_names",
                       "model_numbers")], id = c("row_names", "model_numbers"))
melt_datas

# Plot classification error
plot_list[[1]] <- ggplot(data=melt_datas,
       aes(x=row_names, y=value)) +
       geom_point(aes(colour = as.factor(model_numbers), group = as.factor(model_numbers)), size = 3) +
       geom_line(aes(group = variable)) +
       labs(y = "Missclassification error in range 0 to 1",
            x = "Models",
            title = "Missclassification error for training model and test set",
            caption = "Top - Training model, Bottom - Test set",
            colour = "Model id")
ggsave("per_class_error.png")

# Plot MSE error
plot_list[[2]] <- ggplot(data=melt_datas_mse,
      aes(x=row_names, y=value)) +
      geom_point(aes(colour = as.factor(model_numbers), group = as.factor(model_numbers)), size = 3) +
      geom_line(aes(group = variable)) +
      labs(y = "MSE error",
           x = "Models",
           title = "MSE error for training model and test set",
           caption = "Top - Training model, Bottom - Test set",
           colour = "Model id")
ggsave("mse_error.png")


## dataframe to latex table
latex_df <- results_df[, !names(results_df) %in% c("train_error",
                                                   "train_mse", "row_names")]

latex_df <- latex_df[, c(1,2,3,4,5,6,9,7,8)]
print(xtable(latex_df, display = c("d", "d", "d", "f", "e", "f", "f", "f", "f", "f"),
             digits = c(0, 0, 0, 2, 2, 2, 4, 4, 4, 4)),
             file = "result_data_frames/error_table.tex",
             include.rownames = FALSE )
