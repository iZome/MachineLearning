require(caret)          # useful library to split up data set
library(tikzDevice)     # library to export plots to .tex files
library(xtable)         # library to export data frames to tables in .tex files

options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}", "\\usepackage[T1]{fontenc}",
                               "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))

ggplot_to_latex <- function(
    ggplot,
    destination_path,
    width,
    height
){
    tikz(file = paste0(destination_path, ".tex"), width = 6, height = 4)
    print(ggplot)
    dev.off()
}

create_confusion_matrix <- function(
    predicted_value,
    true_value,
    destination_path
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

    write.csv(x = conf_df, file = paste0(destination_path, "_Confusion_Matrix.csv"))
    print(xtable(conf_df, display = c("s", rep("d", 11), "f", "s"),
                 digits = c(rep(0, 12), 3, 0)),
          #table.placement = "H",
          only.contents = TRUE,
          file = paste0(destination_path ,"_Confusion_Matrix.tex"),
          include.rownames = FALSE)

}
