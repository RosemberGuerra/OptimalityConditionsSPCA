# Optimality Conditions for Penalized Sparse PCA

## Overview

This repository contains code and scripts for conducting numerical experiments related to Optimality Conditions for Penalized Sparse PCA. The experiments explore the trade-offs between variance explained and sparsity and compare different penalties (L1, L0, SCAD).

The main goals of this experiment are:
- Generate synthetic datasets for simulations.
- Implement and evaluate Sparse PCA using alternating optimization.
- Compare the performance of penalty functions via simulation and statistical tests.
- Visualize and interpret the results.

## Contents

### 1. Data Generation
- **Script**: `data_generation.R`
- **Description**: Generates synthetic datasets with varying dimensions and saves them for later use in numerical experiments.
- **Key Parameters**:
  - `n`: Number of observations.
  - `p`: Number of variables.
  - `S`: Number of simulations.
- **Output**: Data matrices saved in `../data/` as text files.

### 2. Implementation
- **Script**: `spca_estimations.R`
- **Description**: Implements the alternating optimization algorithm for Sparse PCA with support for L1, L0, and SCAD penalties.
- **Functions**:
  - `alt_spca`: Alternating optimization for a single sparse component.
  - `alt_spca_multi`: Multivariate extension with deflation.
  - Utility functions for calculating variance, sparsity, and penalties.

### 3. Numerical Experiments
#### Trade-Off Curve Simulation
- **Script**: `trade_off_curve.R`
- **Description**: Simulates and visualizes the trade-off curve between variance explained and sparsity for different penalties.
- **Key Features**:
  - Parallelized execution using `doParallel`.
  - Visualization of results using `ggplot2`.
  - Outputs trade-off plots as PDF files.

#### Permutation Test
- **Script**: `permutation_test.R`
- **Description**: Conducts permutation tests to compare the adjusted variance explained between penalties for datasets with varying sparsity levels.
- **Key Features**:
  - Implements a custom permutation test function.
  - Visualizes p-values for significance testing.

### 4. Results
- **File**: `results1000.RData`
- **Description**: Contains the simulation results for the main numerical experiments.

## Installation

### Prerequisites
The following R libraries are required:
- `dplyr`
- `ggplot2`
- `foreach`
- `doParallel`
- `progressr`

To install the required packages, run:
```R
install.packages(c("dplyr", "ggplot2", "foreach", "doParallel", "progressr"))
