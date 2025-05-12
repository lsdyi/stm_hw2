library(openxlsx)
library(ggplot2)
library(dplyr)
library(DescTools)

library(openxlsx)
library(ggplot2)
library(dplyr)
library(DescTools)

############Question 4###############

# 4a)

a = c(1, 0, 2)
b = c(1, 0, -1)
a %*% b # result = -1
# a and b are not orthogonal because their dot product is equal to -1 instead of 0.

# 4b)

x = rnorm(n=10*3, mean = 0, sd = 1)
X = matrix(x, nrow=10, ncol=3)
b = c(1, 1, 2) # beta
mu = X %*% b

mu[1] # 0.1452579

X[1, ] %*% b # 0.1452579, equal to mu

# 4c)

epsilon = matrix(rnorm(n=10*1, mean=0, sd=0.1), nrow=10, ncol=1)
y = X %*% b + epsilon

b_hat = solve(t(X) %*% X) %*% t(X) %*% y 
b_hat

# 4d)

e = y - X %*% b_hat

n = 10
p = 3
s_squared_error = (t(e) %*% e) / (n-p)
s_squared_error = as.numeric(s_squared_error)

cov_b_hat = s_squared_error * solve(t(X) %*% X)
cov_b_hat

se_b_hat = sqrt(diag(cov_b_hat))
se_b_hat














