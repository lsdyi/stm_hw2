library(openxlsx)
library(ggplot2)
library(dplyr)
library(DescTools)

############Question 6###############

# 6a)
y = data$nBugs # response variable: the number of bugs, a vector with n = 91 observations
X = data[,-1] # 91 x 5 matrix with covariates
X = as.matrix(X) # X was initial a data frame, but we want it to be matrix

loglik_poisreg = function(y, X, betavect){
  lambda = exp(X %*% betavect)
  loglik = sum(dpois(y, lambda, log=TRUE))
  return(loglik)
}

initVal_beta = rep(0, ncol(X))
optres_reg = optim(initVal_beta, loglik_poisreg,
                   y=y, X=X, method='BFGS',
                   control=list(fnscale=-1), hessian=TRUE)

mle_beta_hat = optres_reg$par
names(mle_beta_hat) = colnames(X)
mle_beta_hat

# 6b)

beta_hat_cov = solve(-optres_reg$hessian)
beta_hat_se = sqrt(diag(beta_hat_cov))
names(beta_hat_se) = colnames(X)
beta_hat_se

CI_95_lower = mle_beta_hat - 1.96*beta_hat_se
CI_95_upper = mle_beta_hat + 1.96*beta_hat_se

CI_table = data.frame(
  Estimate = mle_beta_hat,
  StdError = beta_hat_se,
  CI_lower = CI_95_lower,
  CI_upper = CI_95_upper
)
CI_table

# 6c)

x92 = c(1, 10, 0.45, 0.5, 0.89)
y_pred = exp(as.numeric(x92 %*% mle_beta_hat))
y_pred
