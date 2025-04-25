###############################################################
# Alternating Optimization for Sparse PCA and Related Methods #
# Author: RI Guerra Urzola                                    #
# Date: 30/10/2024                                            #
###############################################################

### Penalty Functions ###
# Define penalty functions outside main functions to avoid redundancy
Penalty_functions_sol <- list(
  l1 = function(x, lambda) {
    sign(x) * pmax(abs(x) - lambda, 0)
  },
  l0 = function(x, lambda) {
    x * (abs(x) > lambda)
  },
  scad = function(x, lambda) {
    a <- 3.7
    ifelse(abs(x) <= 2 * lambda, 
           sign(x) * pmax(abs(x) - lambda, 0), 
           ifelse(abs(x) <= a * lambda, 
                  sign(x) * ((a - 1) * abs(x) - a * lambda) / (a - 2), 
                  x * (abs(x) > a * lambda)))
  }
)

### Alternating Sparse PCA Function#######

alt_spca <- function(X, w0 = NULL, alpha, penalty = 'l1', maxiter = 1000, tol = 1e-6) {
  # Perform sparse PCA using alternating optimization
  # Parameters:
  # - X: Data matrix
  # - w0: Initial weights (optional)
  # - alpha: Regularization parameter
  # - penalty: Type of penalty ('l1', 'l0', 'scad')
  # - maxiter: Maximum number of iterations
  # - tol: Convergence tolerance

  # Ensure valid penalty type
  if (!(penalty %in% names(Penalty_functions_sol))) {
    stop("Invalid penalty type. Available options are: 'l1', 'l0', 'scad'")
  }
  if (alpha < 0) stop("alpha must be non-negative.")
  if (alpha == 0) return(list(w = prcomp(X)$rotation[, 1], iter = 0, time = 0))
  
  penaltyfun <- Penalty_functions_sol[[penalty]]
  if (is.null(w0)) w0 <- rnorm(ncol(X))
  
  iter <- 0
  runningtime <- system.time({
    while (iter < maxiter) {
      # z-step: Project and normalize
      z <- X %*% w0
      z <- z / norm(z, type = "2")
      
      # w-step: Apply penalty and normalize
      w <- penaltyfun(t(X) %*% z, alpha)
      if (all(w == 0)) break
      w <- w / norm(w, type = "2")
      
      if (norm(w - w0, type = "2") < tol) break
      
      w0 <- w
      iter <- iter + 1
    }
  })
  return(list(w = w, iter = iter, time = runningtime[[3]]))
}

### Multivariate Sparse PCA with Deflation   #########

alt_spca_multi <- function(X, W0 = NULL, K, alpha, penalty = 'l1') {
  # Perform multivariate sparse PCA with deflation
  if (K > ncol(X)) stop("K cannot exceed the number of columns in X.")
  if (is.null(W0)) W0 <- matrix(rnorm(ncol(X) * K), ncol(X), K)
  if (length(alpha) == 1) alpha <- rep(alpha, K)
  else if (length(alpha) != K) stop("alpha must be a scalar or a vector of length K.")
  
  W <- matrix(0, ncol(X), K)
  for (k in 1:K) {
    W[, k] <- alt_spca(X, w0 = W0[, k], alpha = alpha[k], penalty = penalty)$w
    X <- X - X %*% W[, k] %*% t(W[, k])
  }
  return(W)
}

### Performance Metrics and Utility Functions ######


## Compare sparse PCA patterns to PCA patterns
pca_diff <- function(w, X) {
  nz <- which(w != 0)
  X <- X[, nz]
  w <- w[nz]
  w_pca <- prcomp(X)$rotation[, 1]
  norm(abs(w) - abs(w_pca), type = "2")
}

## Define penalty functions for objective calculations
Penalty_functions <- list(
  l1 = function(x, lambda) lambda * abs(x),
  l0 = function(x, lambda) lambda * (x != 0),
  scad = function(x, lambda) {
    a <- 3.7
    ifelse(abs(x) <= lambda, 
           lambda * sum(abs(x)), 
           ifelse(abs(x) <= a * lambda, 
                  (2 * lambda * a * abs(x) - x^2 - lambda^2) / (2 * (a - 1)), 
                  0.5 * (a + 1) * lambda^2))
  }
)

## Objective function for sparse PCA
space_objective <- function(X, w, alpha, penalty = 'l1') {
  if (!(penalty %in% names(Penalty_functions))) {
    stop("Invalid penalty type.")
  }
  penaltyfun <- Penalty_functions[[penalty]]
  norm(X %*% w, type = "F") - sum(penaltyfun(w, alpha))
}

## Objective function difference with PCA
obj_pca_diff <- function(X, w, alpha = 0, penalty = 'l1') {
  nz <- which(w != 0)
  w_pca <- rep(0, ncol(X))
  w_pca[nz] <- prcomp(X[, nz])$rotation[, 1]
  space_objective(X, w, alpha, penalty) - space_objective(X, w_pca, alpha, penalty)
}

## Variance calculation
variance <- function(X, w) {
  w_pca <- prcomp(X)
  (t(w) %*% t(X) %*% X %*% w) / w_pca$sdev[1]^2
}

## Adjusted variance calculation
adj_variance <- function(X, w) {
  indexw <- which(w != 0)
  X_adj <- X[, indexw]
  w_adj <- prcomp(X_adj)$rotation[, 1]
  w_pca <- prcomp(X)
  (t(w_adj) %*% t(X_adj) %*% X_adj %*% w_adj) / w_pca$sdev[1]^2
}

## Cardinality (number of non-zero elements)
cardinality <- function(w) {
  sum(w != 0)
}

## Permutation Test for Sparse PCA Results
permutation_test <- function(values_A, values_B, n_permutations = 10000, alternative = "two.sided") {
  observed_diff <- mean(values_A) - mean(values_B)
  combined <- c(values_A, values_B)
  n_A <- length(values_A)
  permuted_diffs <- replicate(n_permutations, {
    permuted <- sample(combined)
    mean(permuted[1:n_A]) - mean(permuted[(n_A + 1):length(combined)])
  })
  p_value <- switch(alternative,
                    "two.sided" = mean(abs(permuted_diffs) >= abs(observed_diff)),
                    "greater" = mean(permuted_diffs >= observed_diff),
                    "less" = mean(permuted_diffs <= observed_diff),
                    stop("Invalid alternative argument."))
  list(observed_diff = observed_diff, p_value = p_value)
}
