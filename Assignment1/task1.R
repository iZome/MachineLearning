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


plot1a <- function()
    {
    l0 <- legendres(x,0)
    l1 <- legendres(x,1)
    l2 <- legendres(x,2)
    l3 <- legendres(x,3)
    l4 <- legendres(x,4)
    l5 <- legendres(x,5)
    plot(x,l1, type = "l", col = "blue")
    lines(x,l2, col = "mediumspringgreen")
    lines(x,l3, col = "skyblue")
    lines(x,l4, col = "hotpink2")
    lines(x,l5, col = "orangered")
    lines(x,l0, col = "yellowgreen")
    legend(0.8,0.5, legend = c("L0", "L1", "L2", "L3", "L4", "L5"),
           col = c("yellowgreen", "blue", "mediumspringgreen", "skyblue", "hotpink2", "orangered"), lty=1:2, cex=0.8)
    }

generateUniformValues <- function(n)
    {
      runif(n, min = -1, max =1)
    }

targetFunc1 <- function(
    x,Q_f
    )
    {
        alphas <- generateUniformValues(Q_f+1)
        func <- rep(0, length(x))
        for(q in 0:Q_f){
            func <- func + (alphas[q+1] * x**q)
        }
        func
    }

targetFunc2 <- function(
    x,Q_f
    )
    {
        betas <- generateUniformValues(Q_f+1)
        func <- rep(0,length(x))
        for(q in 0:Q_f){
            func <- func + betas[q+1] * legendres(x, q)#(func / (betas[q] * 2**(q-1)) + l(x,q,q)
        }
        func
    }


plot3tf1tf2 <- function(
    ){
      tf1_1 <- targetFunc1(x,5)
      tf1_2 <- targetFunc1(x,5)
      tf1_3 <- targetFunc1(x,5)

      tf2_1 <- targetFunc2(x,5)
      tf2_2 <- targetFunc2(x,5)
      tf2_3 <- targetFunc2(x,5)

      plot(x,tf1_1, type = "l", col = "blue")
      lines(x,tf1_2, col = "mediumspringgreen")
      lines(x,tf1_3, col = "skyblue")
      legend(0.8,0.5, legend = c("1", "2", "3"),
             col = c("yellowgreen", "blue", "mediumspringgreen"), lty=1:2, cex=0.8)

      plot(x,tf2_1, type = "l", col = "blue")
      lines(x,tf2_2, col = "mediumspringgreen")
      lines(x,tf2_3, col = "skyblue")
      legend(0.8,0.5, legend = c("1", "2", "3"),
             col = c("yellowgreen", "blue", "mediumspringgreen"), lty=1:2, cex=0.8)
    }
plot3tf1tf2()
