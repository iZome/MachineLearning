## Libraries and seed
library(ggplot2)
set.seed(420)
## Help functions

make_data_set <- function(
    expression,
    x
    )
    {
        y_dataset <- sapply(x, function(x) eval(expression))
        return(y_dataset)
    }

## legendre expression within sum formula
l <- function(
    x,q,k
    ){
      legendre <- x**k * choose(q,k) * choose((q+k-1)/2,q)
      return(legendre)
    }

## produces legendre polynomial
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

regularLinearization <- function(
    x,
    data,
    lambda,
    Q_order
    ){
        legendreMatrix <- regularEstimation(data, lambda, Q_order)
        pol <- legendreMatrix[1]
        for(i in 2:(Q_order +1)){
            pol <- pol + (legendreMatrix[i] * (legendres(x, i)))
        }
        return(pol)
    }

generator <- function(
    N,
    x,
    func,
    sigma
    )
    {

    }

## Main functions

task2i <- function()
    {
        N = 50
        x = runif(n = N, min = -1, max = 1)

        y <- quote(sin(pi*x))
        y_wth_noise <- quote(sin(pi*x) + rnorm(1,0,1))

        y_dataset <- make_data_set(y_wth_noise, x)

        ggplot_df <- data.frame(x, y_dataset)

        ggplot1 <- ggplot(data = ggplot_df, aes(x = x)) +
                   geom_point(aes(y = y_dataset, colour = "y with noise")) +
                   geom_line(aes(y = sin(pi*x), colour = "y")) +
                   xlab("x") +
                   ylab("y")
        ggsave("Pictures/task2i.png")
    }

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

        print(lambda0)
        print(lambda5)

        ggplot1 <- ggplot(data = ggplot_df, aes(x = x)) +
                   geom_point(aes(y = y, colour = "y with noise")) +
                   geom_line(aes(y = sin(pi*x), colour = "y")) +
                   geom_line(aes(y = lambda0, colour = "lambda0")) +
                   geom_line(aes(y = lambda5, colour = "lambda5")) +
                   xlab("x") +
                   ylab("y")
        ggsave("Pictures/task2ii.png")
    }

## Run

main <- function()
    {
        task2i()
        task2ii()
    }

main()

## Plotting against the machine
