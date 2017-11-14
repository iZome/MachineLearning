set.seed(58)
#library(polynom)
library(colorRamps)
library(plot3D)


## HELP functions
make_N_legendre <- function(q, x){
    #sum part as for loop
    y <- rep(0, length(x))
    for(k in 0:q){
        legendre_k <- x**k * choose(q, k) * choose((q+k-1)/2,q)
        y <- y +legendre_k
    }
    y <- 2**q * y
    return(y)
}

targetFunction2 <- function(x, beta_set, Q){
    func <- rep(0, length(x))
    for(q in 0:Q){
        func <- func + beta_set[q+1] * make_N_legendre(q, x)
    }
    return(func)
}

generate_uniform_values <- function(n){
    runif(n, min = -1, max = 1)
}

# make a g of order Q
make_model_g <- function(Q, x){
    matrix_legendre <- matrix(0L, nrow = length(x), ncol = Q+1)

    for(i in 0:Q){
        matrix_legendre[, i+1] <- make_N_legendre(i, x)
    }
    return(matrix_legendre[,2:ncol(matrix_legendre)])
}


## TASK 1 i

calculate_e_out_2 <- function(number_new, beta_set_tf, beta_set_g, Q_tf){
    comparison_beta_set <- beta_set_tf - beta_set_g
    new_x_values <- generate_uniform_values(number_new)
    mse <- mean(targetFunction2(new_x_values, comparison_beta_set, Q_tf)**2)
    #print(mse)
    return(mse)
}

find_difference <- function(x, beta_set_tf, beta_set_g){
    diff <- 0  #diff = difference
    for(i in 1:length(beta_set_tf)){
        diff <- diff + (beta_set_tf[i]-beta_set_g[i]) * make_N_legendre(i, x)
    }
    return(diff^2)
}

calculate_e_out <- function(x_tf, y_tf, beta_set_tf, Q_g){
    calculated_coefficients <- lm(y_tf ~ make_model_g(Q_g, x_tf)) #tf = targetfunction, Q = order


    beta_set_g <- unname(coef(calculated_coefficients))

    size_difference_beta <- length(beta_set_tf) - Q_g
    if(size_difference_beta != 0){
        if( size_difference_beta > 0){
            beta_set_g <- c(beta_set_g, rep(0, size_difference_beta))
        }
        else{
            beta_set_tf <- c(beta_set_tf, rep(0, -size_difference_beta))
        }
    }
    # bias_g <- integrate(find_difference, -1, 1, beta_set_tf, beta_set_g)
    # return (bias_g$value)
    error_mse <- calculate_e_out_2(30, beta_set_tf, beta_set_g, length(beta_set_tf) - 1)
    return(error_mse)
}

find_overfit <- function(x_tf, y_tf, beta_set_tf, Q_set){
    err_g_1 <- calculate_e_out(x_tf, y_tf, beta_set_tf, Q_set[1]) #g_1 is first g, in this task g2
    err_g_2 <- calculate_e_out(x_tf, y_tf, beta_set_tf, Q_set[2]) #g_2 is second g, in this task g10

    return(err_g_2 - err_g_1)
}

make_error_matrix_i <- function(N_set, sigma_set, beta_set_tf, Q_tf){
    final_matrix <- matrix(0L, nrow = length(N_set), ncol = length(sigma_set))
    #print(length(sigma_set))

    #iterate through size of data
    for(k in 1:length(N_set)){
        x_tf <- sort(generate_uniform_values(N_set[k])) # tf = target function
        y_tf <- targetFunction2(x_tf, beta_set_tf, Q_tf)
        for(l in 1:length(sigma_set)){
            sigma <- sigma_set[l]
            y_tf <- y_tf + rnorm(n = length(x_tf), mean = 0, sd = sigma**2)
            overfit_measure <- find_overfit(x_tf, y_tf, beta_set_tf, c(2,10))
           if(abs(overfit_measure) > 0.2){
               overfit_measure <- 0.2 * sign(overfit_measure)
            }
            final_matrix[k, l] <- overfit_measure
        }
    }
    return(final_matrix)
}

avg_runs_i <- function(Q_tf, n_averages, N_set, sigma_set){
    avg_matrix <- matrix(0L, nrow = length(N_set), ncol = length(sigma_set))
    pb <- txtProgressBar(min = 0, max = n_averages)
    for(i in 1:n_averages){
        setTxtProgressBar(pb, i)
        beta_set_tf <- runif(Q_tf+1, min = -1, max = 1)
        new_matrix <- make_error_matrix_i(N_set, sigma_set, beta_set_tf, Q_tf)
        matrix_list <- list(avg_matrix*(i-1), new_matrix)
        avg_matrix <- Reduce("+", matrix_list) / i
        print(min(avg_matrix))
        print(max(avg_matrix))
        pdf("error_matrix.pdf")
        image(avg_matrix, zlim = c(-0.2,0.2), col = colorRamps::matlab.like(100))
        dev.off()
        pdf("3D-plot2.pdf")
        persp3D(x = N_set, y = sigma_set, z = avg_matrix, colvar = avg_matrix , clim = c(-0.5,0.5), zlim = c(-0.5,0.5))
        dev.off()
    }
    close(pb)
    return(avg_matrix)
}

taski <- function(){
    Q_tf <- 20 # tf = target function
    beta_set_tf <- runif(Q_tf+1, min = -1, max = 1)
    N <- seq(20, 110, by = 1)
    sigma <- seq(0.2, 1.1, by = 0.02)

    error_matrix <- make_error_matrix_i(N, sigma, beta_set_tf, Q_tf)
    error_matrix <- avg_runs_i(Q_tf, 100, N, sigma)
    print(error_matrix)
    image(error_matrix, zlim = c(-0.5,0.5), col = colorRamps::matlab.like(10))


    ## UNDER: just to show that the fit function actuall works, uncomment and look at the produced plot to clarify :)
    # x_experiment <- sort(generate_uniform_values(100))
    # #plot_model_g(x_experiment, make_model_g(2, x_experiment))
    # y_experiment_underlying <- targetFunction2(x_experiment, beta_set_tf, Q_tf)
    # y_experiment <- y_experiment_underlying + rnorm(n = length(x_experiment), mean = 0, sd = 0.5**2)
    # y_10g <- lm(y_experiment ~ make_model_g(10, x_experiment))
    # print(beta_set_tf)
    # print(y_10g)
    # beta_set_10g <- unname(coef(y_10g))
    # y_10g <- targetFunction2(x_experiment, beta_set_10g, 10)
    # y_compare <- targetFunction2(x_experiment, beta_set_tf - beta_set_10g, 10)
    # y_2g <- lm(y_experiment ~ make_model_g(2, x_experiment))
    # print(y_2g)
    # beta_set_2g <- unname(coef(y_2g))
    # y_2g <- targetFunction2(x_experiment, beta_set_2g, 2)
    #
    # plot(x_experiment, y_experiment_underlying, type = "l", col = "red")
    # lines(x_experiment, y_10g, type = "l", col = "blue")
    # lines(x_experiment, y_2g, type = "l", col = "purple")
    # lines(x_experiment, y_compare, type = "l", col = "orange")
}

plot_model_g <- function(x, model_g){
    plot(x, model_g[,1], type = "l", xlim = c(-1,1), ylim = c(min(model_g), max(model_g)))
    for(i in 2:ncol(model_g)){
        lines(x, model_g[,i])
    }
}



taski()
