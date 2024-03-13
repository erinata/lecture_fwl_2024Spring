# generate data
x1 <- rnorm(1000, 0, 1)
x2 <- rnorm(1000, 0, 1)
y <- 1 + 2*x1 + 3*x2 + rnorm(1000, 0, 1)


# regress y on x1 and x2
lm(y ~ x1 + x2)

# use FWL
# regress y on x2 and get residual A, 
# then regress x1 on x2 and get residual B, 
# regress constant one on x2 and get residual C, 
# egress residual A on residual B and residual C
ones <- c(rep(1, 1000))
eA <- resid(lm(y ~ x2-1))
eB <- resid(lm(x1 ~ x2-1))
eC <- resid(lm(ones ~ x2-1))
lm(eA ~ eC + eB-1)


# Both method give you rhte same beta0 hat and beta1 hat

