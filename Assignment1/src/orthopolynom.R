library(orthopolynom)

(leg4coef <- legendre.polynomials(n=4, normalized=TRUE))

x <- 1:100
leg4 <- as.matrix(as.data.frame(polynomial.values(polynomials=leg4coef,
                                                  x=scaleX(x, u=-1, v=1))))

colnames(leg4) <- c("leg0", "leg1", "leg2", "leg3", "leg4")
leg4 <- leg4[, 2:ncol(leg4)]
leg4

lm(y ~ leg4)
