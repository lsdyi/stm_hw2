library(openxlsx)
library(ggplot2)
library(dplyr)
library(DescTools)

############Bonus Question###############

y = data$nBugs # response variable: the number of bugs, a vector with n = 91 observations
X = data[,-1] # 91 x 5 matrix with covariates
X = as.matrix(X) # X was initial a data frame, but we want it to be matrix

neg_loglik = function(y, X, params){
  r = params[1]
  beta = params[-1]
  mu = exp(X %*% beta)
  
  loglik = sum(dnbinom(y, size=r, mu=mu, log=TRUE))
  
  return(loglik)
}

init_params = c(0.0001, rep(0, ncol(X)))

optres_neg = optim(init_params, neg_loglik, gr=NULL,
                   y=y, X=X, control=list(fnscale=-1),
                   method=c('L-BFGS-B'), hessian=TRUE,
                   lower=c(0.0001, -Inf, -Inf, -Inf, -Inf, -Inf)
                   )
mle_r_hat = optres_neg$par[1]
mle_r_hat

mle_beta_hat = optres_neg$par[-1]
mle_beta_hat

cov_matrix = solve(-optres_neg$hessian)
se_matrix = sqrt(diag(cov_matrix))
se_matrix

r_hat_se = se_matrix[1]
r_hat_se

beta_hat_se = se_matrix[-1]
beta_hat_se
