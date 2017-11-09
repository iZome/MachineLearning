## Libraries and seed
library(ggplot2) #Ggplot library used for plotting
library(tcltk) #Used for loading bar
library(tikzDevice) #Using tikz to store ggplot as tex
set.seed(420)

## Help functions

# calculated function from "quote" expression
make_data_set <- function(
    expression,
    x
    )
    {
        y_dataset <- sapply(x, function(x) eval(expression))
    }

# fit linear model to data with offset
fit_function <- function(
    b_intercept,
    x,
    y,
    N
    )
    {
        g_i <- lm(y ~ 0+x, offset = rep(b_intercept,N))
        slope <- unname(coef(g_i))
    }

## Main functions

# Task 1, fit two linear functions to y with noise
# if i >= 1, performs the average of the runs
# set i = 1 to get only one run
# plot the two estimated functions with the orginal y - function
# also plot relative performance for between the two models
# when averaging many runs, recommended i > 100 for average run
task1i <- function(i)
    {
        N = 30

        sigma = 1
        b_y <- 0.8

        bias_g_1 <- 0
        bias_g_2 <- 0

        b_g_1_avg <- 0
        b_g_2_avg <- 0

        avg_relative_performance <- rep(0, i)

        y_dataset_avg <- rep(0, N)

        y_wthout_noise <- quote(0.8 * x)
        y_wth_noise <- quote(0.8 * x + rnorm(1,0,1))

        for(k in 1:i){

            x <- runif(n = N, min = -1, max = 1)

            y_dataset <- make_data_set(y_wth_noise, x)
            y_dataset_avg <- y_dataset_avg + y_dataset

            b_g_1 <- fit_function(0.5, x, y_dataset, N)
            b_g_2 <- fit_function(-0.5, x, y_dataset, N)

            b_g_1_avg <- b_g_1_avg + b_g_1
            b_g_2_avg <- b_g_2_avg + b_g_2

            bias_g_1 <- bias_g_1 + integrate(function(x) (0.5 + (b_g_1 - b_y) * x)^2, -1, 1)$value
            bias_g_2 <- bias_g_2 + integrate(function(x) (-0.5 + (b_g_2 - b_y) * x)^2, -1, 1)$value

            avg_relative_performance[k] <- bias_g_1 / bias_g_2
        }

        # Task average of all values
        bias_g_1 <- bias_g_1 / i
        bias_g_2 <- bias_g_2 / i
        b_g_1_avg <- b_g_1_avg / i
        b_g_2_avg <- b_g_2_avg / i
        y_dataset <- y_dataset_avg / i

        if(i > 1){
            x = seq(-1,1, length.out = N)
        }

        y_values <- eval(y_wthout_noise)

        g_1 <- quote(0.5 + b_g_1_avg * x)
        g_2 <- quote(-0.5 + b_g_1_avg * x)

        g_1_values <- eval(g_1)
        g_2_values <- eval(g_2)

        print(bias_g_1)
        print(bias_g_2)

        ggplot_df <- data.frame(x, y_dataset, y_values, g_1_values, g_2_values)
        avg_relative_performance_df <- data.frame(x = 1:i, avg_relative_performance)

        # plot fitted function together with orginal function y
        tikz(file = paste0("Pictures/Task1/task1ibias", i, ".tex"), width = 5, height = 5)
        ggplot1 <- ggplot(data = ggplot_df, aes(x = x)) +
                   geom_line(aes(y = y_values, colour = "y")) +
                   geom_line(aes(y = g_1_values, colour = "$g_1$"), linetype = "dashed") +
                   geom_line(aes(y = g_2_values, colour = "$g_2$"), linetype = "dashed") +
                   xlab("x") +
                   ylab("y") +
                   annotate("text", x = -0.1, y = 0.7,
                            label = paste0("$\\mathrm{BIAS}(y,g_1) = ",
                                            round(bias_g_1,3),
                                            "$ \\\\",
                                            "$g_1 = 0.5 + ",
                                            round(b_g_1_avg,3),
                                            " x$")) +
                   annotate("text", x = 0.5, y = -0.4,
                            label = paste0("$\\mathrm{BIAS}(y,g_2) = ",
                            round(bias_g_2,3),
                            "$ \\\\",
                            "$g_2 = -0.5 + ",
                            round(b_g_2_avg,3),
                            " x$"))
        #Only plot datapoints when running one dataset
        if(i == 1){
            ggplot1 <- ggplot1 +
            geom_point(aes(y = y_dataset, colour = "$y_{noise}$")) +
            scale_colour_manual("Legend",
                              breaks = c("$y_{noise}$", "y", "$g_1$", "$g_2$"),
                              values = c("blue", "red", "black", "black"),
                              guide = guide_legend(override.aes = list(
                                  linetype = c("blank", "solid", "dashed", "dashed"),
                                  shape = c(16, NA, NA, NA)
                                  ))) +
            theme(legend.position = c(0.9, 0.2))
        }else{
            ggplot1 <- ggplot1 +
            scale_colour_manual("Legend",
                              breaks = c( "y", "$g_1$", "$g_2$"),
                              values = c("blue", "red", "black"),
                              guide = guide_legend(override.aes = list(
                                  linetype = c( "solid", "dashed", "dashed"),
                                  shape = c( NA, NA, NA)
                                  ))) +
            theme(legend.position = c(0.9, 0.2))
        }

        ggsave(paste0("Pictures/Task1/task1i", i, ".png"))
        print(ggplot1) #need to print plot to write to .tex file
        dev.off()

        # plot average relative performance rp and shows value after averaging 1 to i times
        tikz(file = paste0("Pictures/Task1/task1ibias_avg", i, ".tex"), width = 5, height = 5)
        ggplot2 <- ggplot(data = avg_relative_performance_df, aes(x = x)) +
                   geom_line(aes(y = avg_relative_performance, colour = "$rp_{avg}$")) +
                   xlab("runs") +
                   ylab("$\\mathrm{BIAS}_{avg}$") +
                   theme(legend.position = c(0.8, 0.2))
        ggsave(paste0("Pictures/Task1/task1i_avg", i, ".png"))
        print(ggplot2)
        dev.off()
    }

# finding the val. and out. error for different sizes of training and validation
# set out of a set with total size N = 30
task1ii <- function(
    n_training_sets = 10000
    )
    {
        N = 30
        indexes <- 1:N

        # progress bar
        pb <- tkProgressBar(title = paste0("Running ", n_training_sets, " data sets"),
                            min = 0, max = n_training_sets, width = 300)

        length_df <- N - 2*5 +1
        error_df <- data.frame(g_1_error_val = rep(0,length_df),
                               g_2_error_val = rep(0,length_df),
                               g_1_error_out = rep(0,length_df),
                               g_2_error_out = rep(0,length_df),
                               collection_val = rep(0,length_df),
                               collection_out = rep(0,length_df))

        for(k in 1:n_training_sets){
            setTkProgressBar(pb, k, label = paste(round(k/n_training_sets*100, 0), "% done"))
            x <- runif(n = N, min = -1, max = 1)
            b_y <- 0.8
            y_wth_noise <- quote(0.8 * x + rnorm(1,0,1))
            y_dataset <- make_data_set(y_wth_noise, x)
            for(i in 5:(N-5)){
                train_indexes <- sample(indexes, N - i)
                test_indexes <- subset(indexes, !(indexes %in% train_indexes))

                x_train <- x[train_indexes]
                x_test <- x[test_indexes]

                y_train <- y_dataset[train_indexes]
                y_test <- y_dataset[test_indexes]

                b_g_1 <- fit_function(0.5, x_train, y_train, N - i)
                b_g_2 <- fit_function(-0.5, x_train, y_train, N - i)

                g_1 <- quote(0.5 + b_g_1 * x_test) # TO DO: prøve å utføre fit_function og få ut verde før settes i quote
                g_2 <- quote(-0.5 + b_g_2 * x_test)

                g_1_values <- eval(g_1)
                g_2_values <- eval(g_2)

                g_1_mse <- round(mean((y_test - g_1_values)^2),5)
                g_2_mse <- round(mean((y_test - g_2_values)^2),5)

                g_1_bias <- integrate(function(x) (0.5 + (b_g_1 - b_y) * x)^2, -1, 1)$value + 1
                g_2_bias <- integrate(function(x) (-0.5 + (b_g_2 - b_y) * x)^2, -1, 1)$value +1

                error_df$g_1_error_val[i-4] <- (error_df$g_1_error_val[i-4]  + g_1_mse)
                error_df$g_2_error_val[i-4] <- (error_df$g_2_error_val[i-4]  + g_2_mse)

                error_df$g_1_error_out[i-4] <- (error_df$g_1_error_out[i-4]  + g_1_bias)
                error_df$g_2_error_out[i-4] <- (error_df$g_2_error_out[i-4]  + g_2_bias)

                error_df$collection_val[i-4] <- (error_df$collection_val[i-4] + min(g_1_mse, g_2_mse))
                error_df$collection_out[i-4] <- (error_df$collection_out[i-4] + min(g_2_bias, g_2_bias))
            }
        }
        close(pb)
        error_df$test_set_amount <- 5:(N-5)

        error_df[-7] <- error_df[-7] / n_training_sets

        # Individual errors for g_1 and g_2
        ggplot1 <- ggplot(data = error_df, aes(x = test_set_amount)) +
                   geom_line(aes(y = g_1_error_val, colour = "g_1")) +
                   geom_line(aes(y = g_2_error_val, colour = "g_2")) +
                   geom_line(aes(y = g_1_error_out, colour = "g_1_2")) +
                   geom_line(aes(y = g_2_error_out, colour = "g_2_2"))
        ggsave("Pictures/Task2/task2ii1.png")

        # Best error chosen at each i
        tikz(file = paste0("Pictures/Task1/task1ii.tex"), width = 5, height = 5)
        ggplot2 <- ggplot(data = error_df, aes(x = test_set_amount)) +
                   geom_line(aes(y = collection_val, colour = "$E_{val}$")) +
                   geom_line(aes(y = collection_out, colour = "$E_{out}$")) +
                   xlab("i") +
                   ylab("y") +
                   scale_colour_manual("Legend",
                                     breaks = c("$E_{val}$", "$E_{out}$"),
                                     values = c("blue", "red")) +
                   theme(legend.position = c(0.9, 0.2))
        ggsave("Pictures/Task2/task2ii2.png")
        print(ggplot2)
        dev.off()
    }

## Run

main <- function()
    {
        #for(i in 1:10){
        #    task1i(i)
        #}
        #task1i(20000)
        task1i(1)
        task1ii()
    }

main()

## Plotting against the machine
