

data <- read.csv("dataset_3_obs.csv")

# print(data)
lm(y~x1+x2+x3, data = data)


data <- read.csv("dataset_3_obs.csv")

ones<-c(1,1,1)
eD <- resid(lm(ones~data$x3-1))
print(eD)

eA <- resid(lm(y~x3-1, data = data))
print(eA)
eB <- resid(lm(x1~x3-1, data = data))
print(eB)
eC <- resid(lm(x2~x3-1, data = data))
print(eC)
lm(eA~eD+eB+eC-1)
