########################################################
# Numerical Experiment: Trade-Off Curve of Variance vs. Sparsity #
# Author: RI Guerra Urzola                             #
# Date: 11-12-2024                                     #
########################################################

# Set the working directory to the location of this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required libraries
library(foreach)      # Parallel processing support
library(doParallel)   # Parallel backend for foreach
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(progressr)    # Progress tracking

# Clear the workspace to avoid conflicts with previous variables or functions
rm(list = ls())

# Load external scripts with necessary functions
source('spca_estimations.R')

# Set seed for reproducibility
set.seed(123)

############################################
# Parallel Processing Setup                #
############################################
num_cores <- detectCores() - 2  # Use all but two cores for parallelization
cl <- makeCluster(num_cores)
registerDoParallel(cl)

############################################
# Parameter Initialization                 #
############################################
S <- 1000                          # Number of simulations
n <- 100                           # Number of observations
p <- c(20, 100, 200)               # Number of variables
penalties <- c('l1', 'l0', 'scad') # Types of penalty functions
k <- 1                             # Number of components
n_alpha <- 10                      # Number of regularization parameters
alpha <- seq(from = 0, to = 1.5, by = 1 / n_alpha) # Regularization parameters

# Create a grid of parameters for the simulations
param_grid <- expand.grid(n = n, p = p, s = c(1:S), penalty = penalties, alpha = alpha)
total_iterations <- nrow(param_grid)  # Total number of iterations

############################################
# Results Initialization                   #
############################################
# Create an empty data frame to store the results
results <- data.frame()

############################################
# Simulation Loop with Parallelization     #
############################################
# Perform parallel computation for each parameter set in the grid
results <- foreach(i = 1:total_iterations, .combine = rbind, .packages = c('dplyr')) %dopar% {
  # Source the script that contains the functions (to ensure availability in each worker)
  source('spca_estimations.R')
  
  # Extract parameter values for the current iteration
  n <- param_grid$n[i]
  p <- param_grid$p[i]
  s <- param_grid$s[i]
  
  # Load the corresponding dataset
  X <- read.table(paste0('../data/simdata_', n, '_', p, '_', s, '.txt')) %>% as.matrix()
  
  # Perform sparse PCA
  spca <- alt_spca(X, alpha = param_grid$alpha[i], penalty = param_grid$penalty[i])
  
  # Calculate performance metrics
  pev <- variance(X, spca$w)      # Proportion of variance explained
  pev_adj <- adj_variance(X, spca$w) # Adjusted proportion of variance explained
  card <- cardinality(spca$w)    # Sparsity (number of selected features)
  
  # Return results for the current iteration
  data.frame(param_grid[i, ], pev, card, pev_adj, spca$iter, spca$time)
}

# Stop and close the parallel cluster
stopCluster(cl)

############################################
# Results Post-Processing                  #
############################################
# Convert numeric parameters to factors for plotting
results$n <- as.factor(results$n)
levels(results$n) <- c('n = 100')
results$p <- as.factor(results$p)
levels(results$p) <- c('p = 20', 'p = 100', 'p = 200')
results$penalty <- as.factor(results$penalty)
levels(results$penalty) <- c('L1', 'L0', 'SCAD')

############################################
# Visualization: Variance vs. Sparsity     #
############################################

ggplot(results, aes(x = card, y = pev, color = penalty, shape = penalty)) +
  # geom_hline(yintercept = 100, color = "red", linetype = "dashed") +  # Red horizontal line
  stat_summary(fun = mean, geom = "line", size = .5) +
  stat_summary(fun = mean, geom = "point", size = 1.5) +
  facet_grid(n ~ p, scales = "free_x") +
  scale_color_grey(start = 0.2, end = 0.8) +  # Use grayscale for colors
  scale_shape_manual(values = c(16, 17, 15)) +  # Assign distinct shapes for penalties
  labs(
    # title = 'Trade-off curve of mean variance explained vs. alpha with dispersion',
    x = 'Number of features',
    y = 'Mean Proportion of Variance Explained'
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# save plot as pdf
ggsave("../figures/tradeOff-var-car.pdf", 
       width = 10, height = 6, units = "in")

############################################
# Visualization: adjusted Variance vs. Sparsity     #
############################################
ggplot(results, aes(x = card, y = pev_adj, color = penalty, shape = penalty)) +
  # geom_hline(yintercept = 100, color = "red", linetype = "dashed") +  # Red horizontal line
  stat_summary(fun = mean, geom = "line", size = .5) +
  stat_summary(fun = mean, geom = "point", size = 1.5) +
  facet_grid(n ~ p, scales = "free_x") +
  scale_color_grey(start = 0.2, end = 0.8) +  # Use grayscale for colors
  scale_shape_manual(values = c(16, 17, 15)) +  # Assign distinct shapes for penalties
  labs(
    # title = 'Trade-off curve of mean variance explained vs. alpha with dispersion',
    x = 'Number of features',
    y = 'Mean Proportion of Variance Explained'
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# save plot as pdf
ggsave("../figures/tradeOff-var_adj-car.pdf", 
       width = 10, height = 6, units = "in")

results_filter = results %>% filter(card != c(20,100,200))

ggplot(results_filter, aes(x = card, y = spca.iter, color = penalty, shape = penalty)) +
  # geom_hline(yintercept = 100, color = "red", linetype = "dashed") +  # Red horizontal line
  stat_summary(fun = mean, geom = "line", size = .5) +
  stat_summary(fun = mean, geom = "point", size = 1.5) +
  facet_grid(n ~ p, scales = "free_x") +
  scale_color_grey(start = 0.2, end = 0.8) +  # Use grayscale for colors
  scale_shape_manual(values = c(16, 17, 15)) +  # Assign distinct shapes for penalties
  labs(
    # title = 'Trade-off curve of mean variance explained vs. alpha with dispersion',
    x = 'Number of features',
    y = 'Average Number of Iterations'
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


# save plot as pdf
ggsave("../figures/tradeOff-iter-car.pdf", 
       width = 10, height = 6, units = "in")

results_filter_time = results %>% filter(spca.time < 1 & card != c(20,100,200))

ggplot(results_filter_time, aes(x = card, y = spca.time, color = penalty, shape = penalty)) +
  # geom_hline(yintercept = 100, color = "red", linetype = "dashed") +  # Red horizontal line
  stat_summary(fun = mean, geom = "line", size = .5) +
  stat_summary(fun = mean, geom = "point", size = 1.5) +
  facet_grid(n ~ p, scales = "free_x") +
  scale_color_grey(start = 0.2, end = 0.8) +  # Use grayscale for colors
  scale_shape_manual(values = c(16, 17, 15)) +  # Assign distinct shapes for penalties
  labs(
    # title = 'Trade-off curve of mean variance explained vs. alpha with dispersion',
    x = 'Number of features',
    y = 'Average Time (sec)'
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# save plot as pdf
ggsave("../figures/tradeOff-time-car.pdf", 
       width = 10, height = 6, units = "in")

# save results 
save(results, file =paste0('results',S,'.RData'))




