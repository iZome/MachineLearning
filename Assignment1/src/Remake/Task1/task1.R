set.seed(58)

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

taski <- function(){
    x <- seq(-1, 1, by = 0.01)
    colors <- c("blue", "mediumspringgreen", "skyblue", "hotpink2", "orangered", "yellowgreen")
    l_0 <- make_N_legendre(0, x) # plot 0th order legendre with plot, use lines on rest
    plot(x, l_0, type = "l", col = colors[1], xlim = c(-1,1), ylim = c(-1,1))
    for(order in 1:5){
        l_order <- make_N_legendre(order, x)
        lines(x, l_order, col = colors[order+1])
    }

    legend(0.5,0.5, legend = c(expression("L"[0]), expression("L"[1]), expression("L"[2]),
                               expression("L"[3]), expression("L"[4]), expression("L"[5])),
           col = c("yellowgreen", "blue", "mediumspringgreen", "skyblue", "hotpink2", "orangered"), lty=1:2, cex=0.8)
}

targetFunction1 <- function(x, alpha_set, Q){
    func <- rep(0, length(x))
    for(q in 0:Q){
        func <- func + alpha_set[q+1] * x**q
    }
    return(func)
}

targetFunction2 <- function(x, beta_set, Q){
    func <- rep(0, length(x))
    for(q in 0:Q){
        func <- func + beta_set[q+1] * make_N_legendre(q, x)
    }
    return(func)
}

taskii <- function(){
    x <- seq(-1, 1, by = 0.01)
    Q <- 5
    colors <- c("turquoise", "turquoise1", "turquoise2", "tomato", "tomato1", "tomato2")
    alpha_set <- runif(Q+1, min = -1, max = 1)
    beta_set <- runif(Q+1, min = -1, max = 1)

    tf_1 <- targetFunction1(x, alpha_set, Q)
    tf_2 <- targetFunction2(x, beta_set, Q)
    print(length(tf_1))

    plot(x, tf_1, col = colors[1], type = "l", xlim = c(-1,1), ylim = c(-5,5))
    lines(x, tf_2, col = colors[4])
    for(order in 2:3){
        alpha_set <- runif(Q+1, min = -1, max = 1)
        beta_set <- runif(Q+1, min = -1, max = 1)

        tf_1 <- targetFunction1(x, alpha_set, Q)
        tf_2 <- targetFunction2(x, beta_set, Q)
        lines(x, tf_1, col = colors[order])
        lines(x, tf_2, col = colors[3+order])
    }

    legend(0.5,0.5, legend = c(expression(alpha["set 1"]), expression(alpha["set 2"]), expression(alpha["set 3"]),
                              expression(beta["set 1"]), expression(beta["set 2"]), expression(beta["set 3"])),
           col = colors, lty=1:2, cex=0.8)

}

taski()
taskii()

warnings()
