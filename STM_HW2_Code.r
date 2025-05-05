# Problem 4a

a = c(1,0,2)
b = c(1,0,-1)

a%*%b
# equal to 0
# if a%*%b == 0, a and b will be orthogonal

# Problem 4b

x = matrix(rnorm(10*3,mean=0,sd=1),nrow = 10, ncol = 3)
#x is [10,3]
dim(x)
b = c(1,1,2)
mu = x %*% b
mu

# Problem 4c
epsilon = matrix(rnorm(10*1,mean=0,sd=0.1),nrow = 10,ncol = 1)

y = mu+epsilon

b_hat = solve(t(x)%*%x)%*%t(x)%*%y

# Problem 4d
# 这里使用的是ytrue-yhat

y_hat = x%*%b_hat

e = y-y_hat

s_sq_eps = t(e)%*%e/(10-3)
s_sq_eps = as.numeric(s_sq_eps)

temp = solve(t(x)%*%x)
cov_b_hat = s_sq_eps*temp

s_sq_b_hat = sqrt(diag(cov_b_hat))