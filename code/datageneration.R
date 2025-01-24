# Data set generations for numerical experiments
# RI Guerra Urzola
# 30/10/2024

normal_data = function(n, p, mu, sigma){
  # n: number of samples
  # p: number of features
  # mu: mean of the normal distribution
  # sigma: standard deviation of the normal distribution
  X = matrix(rnorm(n*p, mu, sigma), n, p)
  return(X)
}

