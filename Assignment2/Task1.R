## Libraries and seed
library(ggplot2)
library(tcltk)
set.seed(420)

## Help functions

make_data_set <- function(
    expression,
    x
    )
    {
        y_dataset <- sapply(x, function(x) eval(expression))
    }

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

task1i <- function(i)
    {
        N = 30

        x <- runif(n = N, min = -1, max = 1)

        y_wthout_noise <- quote(0.8 * x)
        y_wth_noise <- quote(0.8 * x + rnorm(1,0,1))

        y_dataset <- make_data_set(y_wth_noise, x)

        g_1 <- quote(0.5 + fit_function(0.5, x, y_dataset, N) * x) # TO DO: prøve å utføre fit_function og få ut verde før settes i quote
        g_2 <- quote(-0.5 + fit_function(-0.5, x, y_dataset, N) * x)

        g_1_values <- eval(g_1)
        g_2_values <- eval(g_2)
        y_values <- eval(y_wthout_noise)

        g_1_mse <- round(mean((y_values - g_1_values)^2),3)
        g_2_mse <- round(mean((y_values - g_2_values)^2),3)

        ggplot_df <- data.frame(x, y_dataset, y_values, g_1_values, g_2_values)

        ggplot1 <- ggplot(data = ggplot_df, aes(x = x)) +
                   geom_point(aes(y = y_dataset, colour = "y with noise")) +
                   geom_line(aes(y = y_values, colour = "y")) +
                   geom_line(aes(y = g_1_values, colour = "g_1")) +
                   geom_line(aes(y = g_2_values, colour = "g_2")) +
                   scale_colour_manual("Legend",
                                     breaks = c("y with noise", "y", "g_1", "g_2"),
                                     values = c("blue", "red", "black", "black")) +
                   xlab("x") +
                   ylab("y") +
                   annotate("text", x = -0.1, y = 0.7, label = paste0("MSE(y,g_1) = ", g_1_mse)) +
                   annotate("text", x = 0.5, y = -0.4, label = paste0("MSE(y,g_2) = ", g_2_mse))

        ggsave(paste0("Pictures/task1i", i, ".png"))
    }

task1ii <- function(
    n_training_sets = 10000
    )
    {
        N = 30
        indexes <- 1:N

        pb <- tkProgressBar(title = paste0("Running ", n_training_sets, " data sets"),
                            min = 0, max = n_training_sets, width = 300)

        length_df <- N - 2*5 +1
        error_df <- data.frame(g_1_error_val = rep(0,length_df), g_2_error_val = rep(0,length_df),
                               g_1_error_out = rep(0,length_df), g_2_error_out = rep(0,length_df),
                               collection_val = rep(0,length_df), collection_out = rep(0,length_df))
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
        print(error_df)

        error_df[-7] <- error_df[-7] / n_training_sets
        print(error_df)

        ggplot1 <- ggplot(data = error_df, aes(x = test_set_amount)) +
                   geom_line(aes(y = g_1_error_val, colour = "g_1")) +
                   geom_line(aes(y = g_2_error_val, colour = "g_2")) +
                   geom_line(aes(y = g_1_error_out, colour = "g_1_2")) +
                   geom_line(aes(y = g_2_error_out, colour = "g_2_2"))
        ggsave("tempMaria.png")

        ggplot2 <- ggplot(data = error_df, aes(x = test_set_amount)) +
                   geom_line(aes(y = collection_val, colour = "E_val")) +
                   geom_line(aes(y = collection_out, colour = "E_out"))
        ggsave("tempHalvor.png")
    }

## Run

main <- function()
    {
        #for(i in 1:10){
        #    task1i(i)
        #}
        task1ii()
    }

main()

## Plotting against the machine
