## Libraries and seed
library(ggplot2)
library(tcltk)
library(tikzDevice)
set.seed(420)
## Help functions

options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))

# Take a expression and compute the values based on input z
make_data_set <- function(
    expression,
    x
    )
    {
        y_dataset <- sapply(x, function(x) eval(expression))
        return(y_dataset)
    }

# legendre expression within sum formula
l <- function(
    x,q,k
    ){
      legendre <- x**k * choose(q,k) * choose((q+k-1)/2,q)
      return(legendre)
    }

# produces legendre polynomial
legendres <- function(
    x,q
    )
    {
    func <- rep(0,length(x))
        for(k in 0:q){
            func <- func + l(x,q,k)
        }
        #print(func)
    legendre_polynomial <- 2**q * func
    return(legendre_polynomial)
    }

# use regularization paramter lambda to train weights use in the model
regularEstimation <- function(
    data,
    lambda,
    Q_order
    )
    {
        dimentions <- dim(data)
        y = matrix(data[,2], nrow = dimentions[1])
        Z = matrix(rep(0,(dimentions[1] * (Q_order + 1))), nrow = dimentions[1])
        for(i in 1:dimentions[1]){
            Z[i,1] <- 1
            for(j in 2:(Q_order+1)){
                Z[i,j] = legendres(data[i, "x"], j-1)
            }
        }
        weights = solve((t(Z)%*%Z) + (lambda * diag(Q_order + 1)))%*%(t(Z)%*%y)
        return(weights)
    }
# use weights from regularEstimation to regularize the legendre based model
regularLinearization <- function(
    x,
    data,
    lambda,
    Q_order
    ){
        legendreMatrix <- regularEstimation(data, lambda, Q_order)
        pol <- legendreMatrix[1]
        for(i in 2:(Q_order +1)){
            pol <- pol + (legendreMatrix[i] * (legendres(x, i-1)))
        }
        return(pol)
    }

# creates n_folds of N indexes from 1 to N
create_cv_idexes <- function(N, n_folds){
    indexes_per_fold <- ceiling(N/n_folds)
    index_matrix <- matrix(0L, nrow = n_folds, ncol = indexes_per_fold)
    index_available <- 1:50
    for(i in 1:n_folds){
        selected_indexes <- sample(index_available, indexes_per_fold)
        index_available <- index_available[! index_available %in% selected_indexes]

        index_matrix[i, ] <- selected_indexes
    }
    return(index_matrix)
}

# calculate the cv error
cv_error <- function(
    N,
    lambda,
    data,
    n_folds,
    cv_indexes,
    Q_order
    )
    {
        indexes <- 1:N
        cv_indexes <- create_cv_idexes(N, n_folds)
        cv_error <- c()
        for(i in 1:n_folds){
            test_indexes <- cv_indexes[i, ]
            train_indexes <- subset(indexes, !(indexes %in% test_indexes))

            x_train <- data$x[train_indexes]
            x_test <- data$x[test_indexes]

            y_train <- data$y[train_indexes]
            y_test <- data$y_real[test_indexes]

            temp_data <- data.frame(x = x_train, y =y_train)

            lambdaX <- regularLinearization(x_test, temp_data, lambda, 10)

            cv_error <- c(cv_error, ((y_test - lambdaX)^2))
        }
        cv_error <- mean(cv_error)
    }

## Main functions

# plotting underlying function with data set
task2i <- function()
    {
        N = 50
        x = runif(n = N, min = -1, max = 1)

        y <- quote(sin(pi*x))
        y_wth_noise <- quote(sin(pi*x) + rnorm(1,0,1))

        y_dataset <- make_data_set(y_wth_noise, x)

        ggplot_df <- data.frame(x, y_dataset)

        tikz(file = "Pictures/Task2/task2i.tex", width = 5, height = 5)
        ggplot1 <- ggplot(data = ggplot_df, aes(x = x)) +
                   geom_point(aes(y = y_dataset, colour = "$y_{noise}$")) +
                   geom_line(aes(y = sin(pi*x), colour = "$y$")) +
                   xlab("x") +
                   ylab("y") +
                   scale_colour_manual("Legend",
                                     breaks = c( "$y_{noise}$", "$y$"),
                                     values = c("black", "black"),
                                     guide = guide_legend(override.aes = list(
                                         linetype = c( "blank", "solid"),
                                         shape = c( 16, NA)
                                         ))) +
                   theme(legend.position = c(0.9, 0.2))
        ggsave("Pictures/Task2/task2i.png")
        print(ggplot1)
        dev.off()
    }

# Fit two legendre polynomial with different regularization paramters
task2ii <- function(
    )
    {
        N = 50
        sigma = 1
        x <- runif(n = N, min = -1, max = 1)
        y <- sin(pi*x) + rnorm(N,0,sigma^2)

        data <- data.frame(x,y)

        lambda0 <- regularLinearization(x, data, 0, 10)
        lambda5 <- regularLinearization(x, data, 5, 10)

        ggplot_df <- data.frame(x,y,lambda0,lambda5)

        tikz(file = "Pictures/Task2/task2ii.tex", width = 5, height = 5)
        ggplot1 <- ggplot(data = ggplot_df, aes(x = x)) +
                   geom_point(aes(y = y, colour = "$y_{noise}$")) +
                   geom_line(aes(y = sin(pi*x), colour = "$y$")) +
                   geom_line(aes(y = lambda0, colour = "$\\lambda_0$"), linetype = "dashed") +
                   geom_line(aes(y = lambda5, colour = "$\\lambda_5$"), linetype = "dashed") +
                   xlab("x") +
                   ylab("y") +
                   scale_colour_manual("Legend",
                                     breaks = c( "$y_{noise}$", "$y$", "$\\lambda_0$", "$\\lambda_5$"),
                                     values = c("blue", "red", "black", "black"),
                                     guide = guide_legend(override.aes = list(
                                         linetype = c( "blank", "solid", "dashed", "dashed"),
                                         shape = c( 16, NA, NA, NA)
                                         ))) +
                   theme(legend.position = c(0.9, 0.2))
        ggsave("Pictures/Task2/task2ii.png")
        print(ggplot1)
        dev.off()
    }

# plot result from task 2 iii when finding the best regularization parameter
plot_task2iii <- function
    (
    ggplot_df
    )
    {
        lowest_error = ggplot_df[which.min(ggplot_df[,2]),]
        str(lowest_error)
        ?tikzTest
        tikz(file = "Pictures/Task2/task2iii.tex", width = 5, height = 5)
        ggplot1 <- ggplot(data = ggplot_df, aes(x = lambdas, y = error_vector)) +
                   geom_line() +
                   geom_point(data = lowest_error,
                              aes(x = lambdas, y = error_vector),
                              color = "red") +
                    geom_text(data = lowest_error,
                              aes(label = paste0("$\\mathrm{CV}_{error}(\\lambda_{",
                                                  lambdas, "}) = ",
                                                  round(error_vector,4), "$")),
                                   hjust = 0.4, vjust = 1.3) +
                    labs(x = "$\\lambda$", y = "$\\mathrm{CV}_{error}$")
                    #     title = paste("CV error- regularisation $\\lambda$ between",
                    #                    ggplot_df[1,"lambdas"], "and",
                    #                    ggplot_df[nrow(ggplot_df), "lambdas"]))
        print(ggplot1)
        dev.off()
        ggsave("Pictures/Task2/avg_2iii1000runs.png")
    }

# Using CV-error to fund the best regularization parameter for dataset made by
# this model
task2iii <- function(

    )
    {
        N = 50
        sigma = 1
        x <- runif(n = N, min = -1, max = 1)
        y <- sin(pi*x) + rnorm(N, 0, sigma)
        y_real <- sin(pi*x)

        data <- data.frame(x, y, y_real)

        lambda = 0.1
        lambdas <- seq(from = 0.1, 10, by = 0.1)
        n_folds = 10
        error_vector <- integer(length(lambdas))
        cv_indexes <- create_cv_idexes(N, n_folds)

        pb <- tkProgressBar(title = paste0("Running ", length(lambdas), " lambdas"),
                            min = 0, max = length(lambdas), width = 300)
        for(i in 1:length(lambdas)){
            setTkProgressBar(pb, i, label = paste(round(i/length(lambdas)*100, 0), "% done"))
            lambda = lambdas[i]
            print(lambda)
            error_vector[i] <- cv_error(N = N, lambda = lambdas[i], data = data,
                                        n_folds = 10, cv_indexes = cv_indexes,
                                        Q_order = 10)
        }
        close(pb)
        write.csv(error_vector, "error_01")
        print(error_vector)
        ggplot_df <- data.frame(lambdas, error_vector)
        write.csv(ggplot_df, "ggplot_01.csv", row.names = FALSE)
        plot_task2iii(ggplot_df)
    }

# averaging the CV-error for the different lambdas over n_runs dataset
# takes some hours to run on large amounts of dataset,
# if using this more parallelization would be a good step to improve performance
task2iii_avg_runs <- function(
    n_runs
    )
    {
        N = 50
        sigma = 1
        n_folds = 10
        lambdas <- seq(from = 0.1, 10, by = 0.1)
        error_vector <- integer(length(lambdas))
        cv_indexes <- create_cv_idexes(N, n_folds)
        error_result <- error_vector

        pb <- txtProgressBar(min = 0, max = n_runs, style = 3)
        for(r in 1:n_runs){
            setTxtProgressBar(pb, r)
            x <- runif(n = N, min = -1, max = 1)
            y <- sin(pi*x) + rnorm(N, 0, sigma)
            y_real <- sin(pi*x)

            data <- data.frame(x, y, y_real)

            for(i in 1:length(lambdas)){
                error_vector[i] <- cv_error(N = N, lambda = lambdas[i], data = data,
                                            n_folds = 10, cv_indexes = cv_indexes,
                                            Q_order = 10)
            }
            error_result <- error_result + error_vector
        }
        close(pb)
        error_result <- error_result / n_runs
        png("test_avg_2ii.png")
        plot(error_result, type = "b")
        dev.off()
        avg_2iii <- data.frame(lambdas, error_result)
        write.csv(avg_2iii, "avg_2iii.csv", row.names = FALSE)
    }

# plot the best lambda found, a simplification of the plot for ii
task2iii_plot_best_lambda <- function(
    best_lambda
    )
    {
        N = 50
        sigma = 1
        x <- runif(n = N, min = -1, max = 1)
        y <- sin(pi*x) + rnorm(N,0,sigma^2)

        data <- data.frame(x,y)

        lambda_best <- regularLinearization(x, data, best_lambda, 10)

        ggplot_df <- data.frame(x,y,lambda_best)

        tikz(file = "Pictures/Task2/task2iii_best_lambda.tex", width = 5, height = 5)
        ggplot1 <- ggplot(data = ggplot_df, aes(x = x)) +
                   geom_point(aes(y = y, colour = "$y_{noise}$")) +
                   geom_line(aes(y = sin(pi*x), colour = "$y$")) +
                   geom_line(aes(y = lambda_best,
                                 colour = paste0("$\\lambda_", best_lambda, "$")),
                                 linetype = "dashed") +
                   xlab("x") +
                   ylab("y") +
                   scale_colour_manual("Legend",
                                     breaks = c( "$y_{noise}$", "$y$",  paste0("$\\lambda_", best_lambda, "$")),
                                     values = c("blue", "black", "black"),
                                     guide = guide_legend(override.aes = list(
                                         linetype = c( "blank", "solid", "dashed"),
                                         shape = c( 16, NA, NA)
                                         ))) +
                   theme(legend.position = c(0.9, 0.2))
        ggsave("Pictures/Task2/task2iii_best_lambda.png")
        print(ggplot1)
        dev.off()
    }
## Run

main <- function()
    {
        task2i()
        task2ii()
        #task2iii()

        task2iii_avg_runs(1000)

        #big rung for iii, stored to save computation if replot is nessesary
        #ggplot_df <- read.csv("avg_2iii1000runs.csv")
        #colnames(ggplot_df)[2] <- "error_vector"
        #plot_task2iii(ggplot_df)
        task2iii_plot_best_lambda(3.5)
    }

main()
