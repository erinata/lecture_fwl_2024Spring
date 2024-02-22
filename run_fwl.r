generate_sample <-function(n, mu_x1, mu_x2, sigma_x1, sigma_x2, mu_epsilon, sigma_epsilon, beta_0, beta_1, beta_2) {
  x1 <- rnorm(n, mu_x1, sigma_x1)
  x2 <- rnorm(n, mu_x2, sigma_x2)
  epsilon <- rnorm(n, mu_epsilon, sigma_epsilon)
  y <- beta_0 + beta_1*x1 + beta_2*x2 + epsilon
  data.frame (y=y, x1=x1, x2=x2)
}


generate_estimate <-function(dataset){
  n <- dim(dataset)[1]
  y <- matrix(dataset$y, ncol=1)
  X <- matrix(c(rep(1, n), dataset$x1,dataset$x2), ncol=3)
  # p <- dim(X)[2]

  beta_hat <- solve(t(X)%*%X)%*%(t(X)%*%y)
  beta_hat[[3]]  
}

generate_estimate_fwl <-function(dataset){
  n <- dim(dataset)[1]
  y <- matrix(dataset$y, ncol=1)
  X1 <- matrix(c(rep(1, n), dataset$x1), ncol=2)
  X2 <- matrix(dataset$x2, ncol=1)
  
  M1 <- diag(n) - (X1%*%solve(t(X1)%*%X1)%*%t(X1))
  
  beta_2_hat <- solve(t(X2)%*%M1%*%X2)%*%t(X2)%*%M1%*%y
  beta_2_hat[[1]]
}
  

generate_estimate_fwl_x2star_ystar <-function(dataset){
  n <- dim(dataset)[1]
  y <- matrix(dataset$y, ncol=1)
  X1 <- matrix(c(rep(1, n), dataset$x1), ncol=2)
  X2 <- matrix(dataset$x2, ncol=1)
  
  M1 <- diag(n) - (X1%*%solve(t(X1)%*%X1)%*%t(X1))

  beta_2_hat <- solve(t(M1%*%X2)%*%(M1%*%X2))%*%(t(M1%*%X2)%*%M1%*%y)
  beta_2_hat[[1]]
}  

  
  

n <- 1000
mu_x1 <- 1
mu_x2 <- 2
sigma_x1 <- 10
sigma_x2 <- 20
mu_epsilon <- 0
sigma_epsilon <- 1
beta_0 <- 1
beta_1 <- 2
beta_2 <- -3

dataset <- generate_sample(n, mu_x1, mu_x2, sigma_x1, sigma_x2, mu_epsilon, sigma_epsilon, beta_0, beta_1, beta_2)
generate_estimate(dataset)  
generate_estimate_fwl(dataset)  
generate_estimate_fwl_x2star_ystar(dataset)







