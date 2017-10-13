set.seed(1234)

## legendre expression within sum formula
l <- function(
    x,q,k
    ){
      x**k * choose(q,k) * choose((q+k-1)/2,q)
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
    func <- 2**q * func
    }

x = seq(-1,1, by = 0.01)

## plot legendre polynomial of 0 to 5th degree
plot1a <- function()
    {
    png("task1_plots/task1i.png")
    l0 <- legendres(x,0)
    l1 <- legendres(x,1)
    l2 <- legendres(x,2)
    l3 <- legendres(x,3)
    l4 <- legendres(x,4)
    l5 <- legendres(x,5)
    plot(x,l1, type = "l", col = "blue", ylab = expression(italic("y")), xlab = expression(italic("x")))
    lines(x,l2, col = "mediumspringgreen")
    lines(x,l3, col = "skyblue")
    lines(x,l4, col = "hotpink2")
    lines(x,l5, col = "orangered")
    lines(x,l0, col = "yellowgreen")
    legend(0.5,0.5, legend = c(expression("L"[0]), expression("L"[1]), expression("L"[2]),
                               expression("L"[3]), expression("L"[4]), expression("L"[5])),
           col = c("yellowgreen", "blue", "mediumspringgreen", "skyblue", "hotpink2", "orangered"), lty=1:2, cex=0.8)
    dev.off()
    }

## generate uniform values between -1 and 1
generateUniformValues <- function(n)
    {
      runif(n, min = -1, max =1)
    }

## target function 1
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

## target function 2
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

## plot target function 1 and 2
plot3tf1tf2 <- function(
    ){
      tf1_1 <- targetFunc1(x,5)
      tf1_2 <- targetFunc1(x,5)
      tf1_3 <- targetFunc1(x,5)

      tf2_1 <- targetFunc2(x,5)
      tf2_2 <- targetFunc2(x,5)
      tf2_3 <- targetFunc2(x,5)

      png("task1_plots/task1iialpha.png")
      plot(x,tf1_1, type = "l", col = "blue", ylim = c(-2,1), ylab = expression(italic("y")), xlab = expression(italic("x")))
      lines(x,tf1_2, col = "mediumspringgreen")
      lines(x,tf1_3, col = "skyblue")
      legend(0.5,0.5, legend = c(expression(alpha["set 1"]), expression(alpha["set 2"]), expression(alpha["set 3"])),
             col = c("blue", "mediumspringgreen", "skyblue" ), lty=1:2, cex=0.8)
      dev.off()

      png("task1_plots/task1iibeta.png")
      plot(x,tf2_1, type = "l", col = "blue", ylim = c(-3.5,0.5), ylab = expression(italic("y")), xlab = expression(italic("x")))
      lines(x,tf2_2, col = "mediumspringgreen")
      lines(x,tf2_3, col = "skyblue")
      legend(0.5,0.2, legend = c(expression(beta["set 1"]), expression(beta["set 2"]), expression(beta["set 3"])),
             col = c("blue", "mediumspringgreen", "skyblue"), lty=1:2, cex=0.8)
      dev.off()

      tf_40 <- targetFunc1(x,40)
      tf_40_2 <- targetFunc2(x,40)
      png("legendre40.png")
      plot(x,tf_40, type = "l", col = "blue")
      lines(x,tf_40_2, type = "l", col = "red")
      dev.off()
    }
plot1a()
plot3tf1tf2()
