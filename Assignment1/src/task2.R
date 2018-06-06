library(plot3D)
library(lattice)
library(Matrix)
library(ggplot2)
library(reshape2)    # for melt(...)
library(grid)        # for unit(...)
library(colorRamps)

set.seed(1234)

## inner expression of sum formula in Legendre polynom
l <- function(
    x,q,k
    ){
      x**k * choose(q,k) * choose((q+k-1)/2,q)
    }

## print legndre coefficients
l_coeff <- function(
    q,k
    )
    {
      print(choose(q,k) * choose((q+k-1)/2,q))
    }

## legendre polynomial
legendres <- function(
    x,q
    )
    {
    func <- rep(0,length(x))
        for(k in 0:q){
            func <- func + l(x,q,k)
        }
    func <- 2**q * func
    }

## generate n values from uniform distribution with limits [-1,1]
generateUniformValues <- function(n)
    {
      runif(n, min = -1, max =1)
    }

## target function used, return corresponding y-values based on input x, order Q_f and betas
targetFunc2 <- function(
    x,
    Q_f,
    betas
    )
    {
        #betas <- generateUniformValues(Q_f+1)
        func <- rep(0,length(x))
        for(q in 0:Q_f){
            func <- func + betas[q+1] * legendres(x, q)#(func / (betas[q] * 2**(q-1)) + l(x,q,q)
        }
        func
    }

## produce legendre polynomial values for the legendre polynomials from the target function
makeModelg <- function(
    order,
    x
    )
    {
        require(polynom)
        betas <- numeric(order)
        value_matrix <- matrix(0L, nrow = length(x), ncol = order+1)
        fitted_values <- matrix(0L, nrow = length(x), ncol = order+1)

        for(i in 0:order){
            value_matrix[,i+1] <- legendres(x, i)
        }

        value_matrix[,2:ncol(value_matrix)]
    }

## measure the overfit between a 10th- and 2nd order legendre polynomial
measureOverfit <- function(
    x_values,
    y_values,
    order1 = 10,
    order2 = 2
    )
    {
      split_point <- floor(length(x_values)/(5/3))
      x_train_indexes <- which(x_values %in% sample(x_values,split_point))
      x_train_data <- x_values[x_train_indexes]
      y_train_data <- y_values[x_train_indexes]

      x_test_data <- x_values[!(x_values %in% x_values[x_train_indexes])]
      y_test_data <- y_values[!(x_values %in% x_values[x_train_indexes])]

      y_2g <- lm(y_train_data ~ makeModelg(2,x_train_data))
      y_10g <- lm(y_train_data ~ makeModelg(10,x_train_data))

      betasg2 <- unname(coef(y_2g))
      betasg10 <- unname(coef(y_10g))
      betasg2[is.na(betasg2)] <- 0
      betasg10[is.na(betasg10)] <- 0

      g10_y <- targetFunc2(x_test_data, order1, betasg10)
      g2_y <- targetFunc2(x_test_data, order2, betasg2)

      #plot_run(x_values, y_values, x_test_data, g10_y, g2_y)

      err_g10 <- y_test_data - g10_y
      err_g2 <- y_test_data - g2_y

      meanSE_g10 <- sum(err_g10^2)/length(g10_y)
      meanSE_g2 <- sum(err_g2^2)/length(g2_y)

      E <- (meanSE_g10 - meanSE_g2)
      E <- E
      E
    }

makeErrorMatrix <- function(
    N,
    sigma,
    betas10order
    ){
      m <- matrix(0L, nrow = length(N), ncol = length(sigma))
      for(k in 1:length(N)){
        x_values <- generateUniformValues(N[k])
        x_values <- sort(x_values)
        y_values <- targetFunc2(x_values, 10, betas10order)
          for(l in 1:length(sigma)){
              y_values <- y_values + rnorm(n = length(x_values), mean = 0, sd = sigma[l]^2)
              m[k,l] <- measureOverfit(x_values, y_values )
          }
      }
      m
    }

avg_runs <- function(
    n_avgs,
    N,
    sigma
    )
    {
      m <- matrix(0L, nrow = length(N), ncol = length(sigma))
      for(i in 1:n_avgs){
          betas10order <- generateUniformValues(11)
          new_m <- makeErrorMatrix(N, sigma, betas10order)
          matrix_list <- list(m*(i-1), new_m)
          m <- Reduce("+", matrix_list) / i
          m
      }
      print(m)
    }

task2i <- function()
    {

      betas10order <- generateUniformValues(11)

      N = seq(20,110, by=1)
      sigma = seq(0.2, 1.1, by=0.04)

      x <- generateUniformValues(60)
      x <- sort(x)
      # b10order <- targetFunc2(x, 10, betas10order)
      # bg2 <- targetFunc2(x, 2, betasg2)
      # bg10 <- targetFunc2(x, 10, betasg10)
      # print(N)
      # plot(x, b10order, col = "blue")
      # lines(x, bg2,type ="p", col = "pink")
      # lines(x, bg10, type = "p", col = "yellow")

      m_error2 <- avg_runs(20, N, sigma)

      colnames(m_error2) <- sigma
      rownames(m_error2) <- N

      plot_g10_minus_g2(m_error2, N, sigma)

    }

## plot overfit error matrix with the different sigmas and N's on the y and x axis
plot_g10_minus_g2 <- function(
    m_error,
    N,
    sigma
    )
    {
      gg <- melt(m_error)

      ggplot(gg,aes(x=Var1,y=Var2))+
          #geom_tile(aes(fill = value)) +
          geom_raster(aes(fill = value)) +
          scale_fill_gradientn(colours = topo.colors(10), limits = c(-2,2)) +
          labs(x="N", y=expression(sigma), title="Matrix") +
          theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=11),
                     plot.title=element_text(size=11))
      ggsave("smoothedGrid.pdf")

      ggplot(gg,aes(x=Var1,y=Var2))+
          geom_tile(aes(fill = value)) +
          scale_fill_gradientn(colours = colorRamps::matlab.like2(10), limits = c(-4,10)) +
          labs(x="N", y=expression(sigma), title="Matrix") +
          theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=11),
                     plot.title=element_text(size=11))
      ggsave("normalGrid.pdf")

      pdf("3D-plot.pdf")
            persp3D(x = N, y = sigma, z = m_error, colvar = m_error , clim = c(-10,10), zlim = c(-10,10))
      dev.off()

    }

#optional, good for viewing if the function fits are good or not
plot_run <- function(
    x_values,
    y_values,
    x_test_data,
    g10_y,
    g2_y
    )
    {
      plot(x_values, y_values,type = "b", col = "green")
      lines(x_test_data, g10_y, type = "b", col = "hotpink2")
      lines(x_test_data, g2_y, type="b", col = "yellowgreen")
      legend(0.8,0.5, legend = c("The orginal", "g10", "g2"), col = c("green", "hotpink2", "yellowgreen"), lty = 1:2, cex=0.8)
    }

tmp_func <- function()
    {
      betas10order <- generateUniformValues(11)
      x <- generateUniformValues(50)
      x <- sort(x)

      split_point <- floor(length(x)/2)

      x_train_indexes <- which(x %in% sample(x,split_point))
      x_train <- x[x_train_indexes]
      x_test <- x[!(x %in% x[x_train_indexes])]
      print(length(x_train))
      print(length(x_test))

      b10order <- targetFunc2(x, 10, betas10order)

      b10order_train <- b10order[x_train_indexes]
      b10order_test <- b10order[!x %in% x[x_train_indexes]]
      print(length(b10order_train))
      print(length(b10order_test))

      fit_order <- 2
      y <- lm(b10order_train ~ makeModelg(fit_order, x_train))

      fitted_betas <- c( unname(coef(y)))
      fitted_betas[is.na(fitted_betas)] <- 0
      print(fitted_betas)
      print(coef(y))
      y2 <- targetFunc2(x_test, fit_order, fitted_betas)

      png("fit2.png")
      par(mfrow = c(1,2))
      plot(x_test,y2,type = "b", col = "red")
      plot(x,b10order, type = "b")
      dev.off()
      png("fit3.png")
      plot(x_test,y2,type = "b", col = "red", ylim = c(-1,1.6))
      lines(x_train,b10order_train, type = "b")
      dev.off()
    }
tmp_func()

task2i()
