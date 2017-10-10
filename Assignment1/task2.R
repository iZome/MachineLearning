library(plot3D)
set.seed(1234)
l <- function(
    x,q,k
    ){
      x**k * choose(q,k) * choose((q+k-1)/2,q)
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
    x,
    betas
    )
    {

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
      g10_y <- targetFunc2(x_values, order1, betasg10) + rnorm(n = length(x_values), mean = 0, sd = sigma_squared)
      g2_y <- targetFunc2(x_values, order2, betasg2) + rnorm(n = length(x_values), mean = 0, sd = sigma_squared)

      err_g10 <- y_values - g10_y
      err_g2 <- y_values - g2_y

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
        y_values <- targetFunc2(x_values, 10, betas10order)
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

      normalErrors <- rnorm(n = length(N), mean = 0, sd = sigma[1])
      m_error <- makeMatrix(N,sigma, betas10order, betasg10, betasg2)
      persp(x = N,y =sigma,z = m_error)
      contour(x = N, y = sigma, z = m_error)
      image(x = N, y = sigma, z = m_error)
      persp3D(x = N, y = sigma, z = m_error)
      persp3D(z = m_error, shade = 0.3, col = gg.col(100))

    }

task2i()
