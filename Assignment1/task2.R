library(plot3D)
set.seed(1234)
l <- function(
    x,q,k
    ){
      x**k * choose(q,k) * choose((q+k-1)/2,q)
    }

l_coeff <- function(
    q,k
    )
    {
      print(choose(q,k) * choose((q+k-1)/2,q))
    }

legendres <- function(
    x,q
    )
    {
    func <- rep(0,length(x))
        for(k in 0:q){
            func <- func + l(x,q,k)
        }
        #print(func)
    func <- 2**q * func
    }

x = seq(-1,1, by = 0.01)

generateUniformValues <- function(n)
    {
      runif(n, min = -1, max =1)
    }

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

test <- generateUniformValues(11)
tar10 <- targetFunc2(x,10, test)

plot(x,tar10, type = "l", col = "Red", pch = 10)


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
            #str(value_matrix[,i+1])
            #str(x)
        }
        #for(i in 0:order){
          #fitted_values[,i+1] <- predict(as.double(value_matrix[,i+1]),x)
        #}
        #print(value_matrix)
        #print(fitted_values)
        value_matrix[,2:ncol(value_matrix)]
    }

measureOverfit <- function(
    x_values,
    y_values,
    betasg2,
    betasg10,
    sigma,
    order1 = 10,
    order2 = 2
    )
    {
      sigma_squared <- sigma^2

      split_point <- floor(length(x_values)/2)
      x_train_data <- x_values[1:split_point]
      y_train_data <- y_values[1:split_point]

      x_test_data <- x_values[(split_point+1):length(x_values)]
      y_test_data <- y_values[(split_point+1):length(y_values)]

      y_2g <- lm(y_train_data ~ makeModelg(2,x_train_data))
      y_10g <- lm(y_train_data ~ makeModelg(10,x_train_data))

      betasg2 <- unname(coef(y_2g))
      betasg10 <- unname(coef(y_10g))

      g10_y <- targetFunc2(x_test_data, order1, betasg10)
      g2_y <- targetFunc2(x_test_data, order2, betasg2)

      err_g10 <- y_test_data - g10_y
      err_g2 <- y_test_data - g2_y

      meanSE_g10 <- sum(err_g10^2)/length(g10_y)
      meanSE_g2 <- sum(err_g2^2)/length(g2_y)

      E <- abs(meanSE_g10 - meanSE_g2)
      E
    }

makeMatrix <- function(
    N,
    sigma,
    betas10order,
    betasg10,
    betasg2
    ){
      #print(N)
      print(sigma)
      print("yo")
      m <- matrix(0L, nrow = length(N), ncol = length(sigma))
      for(k in 1:length(N)){
        x_values <- generateUniformValues(N[k])
        y_values <- targetFunc2(x_values, 10, betas10order) + rnorm(n = length(x_values), mean = 0, sd = sigma^2)
          for(l in 1:length(sigma)){
              m[k,l] <- measureOverfit(x_values, y_values, betasg2, betasg10, sigma[l])
          }
      }
      m
    }

task2i <- function()
    {

      betas10order <- generateUniformValues(11)

      betasg2 <- generateUniformValues(3)
      betasg10 <- generateUniformValues(11)

      N = seq(20,110, by=1)
      sigma = seq(0.2, 1.1, by=0.0042)

      x <- generateUniformValues(60)
      b10order <- targetFunc2(x, 10, betas10order)
      bg2 <- targetFunc2(x, 2, betasg2)
      bg10 <- targetFunc2(x, 10, betasg10)
      plot(x, b10order, col = "blue")
      lines(x, bg2,type ="p", col = "pink")
      lines(x, bg10, type = "p", col = "yellow")
      normalErrors <- rnorm(n = length(N), mean = 0, sd = sigma[1])
      m_error <- makeMatrix(N,sigma, betas10order, betasg10, betasg2)


      persp(x = N,y =sigma,z = m_error)
      contour(x = N, y = sigma, z = m_error)
      image(x = N, y = sigma, z = m_error)
      persp3D(x = N, y = sigma, z = m_error, colvar = m_error, clim = c(-5,5))
      persp3D(z = m_error, shade = 0.3, col = gg.col(100))

    }

for(i in 0:1){
    print(i)
    l_coeff(1,i)
}

tmp_func <- function()
    {
      betas10order <- generateUniformValues(11)
      x <- generateUniformValues(50)
      x <- sort(x)

      split_point <- floor(length(x)/2)

      x_train_indexes <- which(x %in% sample(x,split_point))
      print(x_train_indexes)
      x_train <- x[x_train_indexes]
      x_test <- x[!x %in% x_train_indexes]
      print(x_train)
      print(x_test)

      b10order <- targetFunc2(x, 10, betas10order)

      b10order_train <- b10order[x_train_indexes]
      b10order_test <- b10order[!x %in% x_train_indexes]

      fit_order <- 10
      y <- lm(b10order_train ~ makeModelg(fit_order, x_train))

      fitted_betas <- c( unname(coef(y)))
      fitted_betas[is.na(fitted_betas)] <- 0
      print(fitted_betas)
      print(coef(y))
      y2 <- targetFunc2(x_test, fit_order, fitted_betas)

      png("fit2.png")
      par(mfrow = c(1,2))
      plot(x_test,y2,type = "p", col = "red")
      plot(x,b10order, type = "p")
      dev.off()
      png("fit3.png")
      plot(x_test,y2,type = "p", col = "red", ylim = c(-1,1.6))
      lines(x_test,b10order_test, type = "p")
      dev.off()
    }
tmp_func()

#task2i()
