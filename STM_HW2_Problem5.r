library(openxlsx)
library(ggplot2)
library(dplyr)
library(DescTools)

############Question 5###############

# 5a)

data = read.csv("https://github.com/StatisticsSU/STM/raw/main/assignment/bugs.csv",
                header = TRUE)
y = data$nBugs # response variable: the number of bugs, a vector with n = 91 observations
X = data[,-1] # 91 x 5 matrix with covariates
X = as.matrix(X) # X was initial a data frame, but we want it to be matrix

mu = 5.2528

loglik_nbinom = function(y, r){
  if (r<=0) return(-Inf)
  loglik = sum(dnbinom(y, size=r, mu=5.2528, log=TRUE))
  return(loglik)
}

initVal_r = c(r=0.0001)
optres_r = optim(initVal_r, loglik_nbinom, gr=NULL, y=y,
               control=list(fnscale=-1), method=c('L-BFGS-B'),
               hessian=TRUE, lower=0.0001)

mle_r_hat = optres_r$par # 1.473745
mle_r_hat

# 5b)

mle_r_se = 1/sqrt(-optres_r$hessian[1])
mle_r_se # 0.2874746

# 5c)

loglik_nbinom = function(y, par){
  r = par[1]
  mu = par[2]
  if (r<=0) return(-Inf)
  loglik = sum(dnbinom(y, size=r, mu=mu, log=TRUE))
  return(loglik)
}

initVal_par = c(r=0.0001, mu=0.0001)
optres_r_mu = optim(initVal_par, loglik_nbinom, gr=NULL,
                    y=y, control=list(fnscale=-1), method=c('L-BFGS-B'),
                    hessian=TRUE, lower=c(0.0001, 0.0001))

mle_r_hat = optres_r_mu$par[1]
mle_r_hat # 1.473744

mle_mu_hat = optres_r_mu$par[2]
mle_mu_hat # 5.252749

# 5d)
cov_mat = solve(-optres_r_mu$hessian)
r_hat_se = sqrt(cov_mat[1, 1])
r_hat_se # 0.2874741

mu_hat_se = sqrt(cov_mat[2, 2])
mu_hat_se # 0.513282

