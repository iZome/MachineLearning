legendres <- function(
    x,q
    )
    {
    func <- rep(0,length(x))
        for(k in 0:q){
            func <- func + (x**k * choose(q,k) * choose((q+k-1)/2,q))
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
      runif(n,-1,1)
    }

targetFunc1 <- function(
    x,Q_f
    )
    {
        alphas <- generateUniformValues(q)
        func <- rep(0, length(x))
        for(q in 0:Q_f){
            
        }
    }
