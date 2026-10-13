# Optimality Conditions for Penalized Sparse PCA

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%E2%89%A54.0.0-blue.svg)](https://www.r-project.org/)


**Replication repository for numerical simulations and optimality benchmarks in penalized Sparse PCA.**

This repository provides reproducible code for the simulation studies and numerical experiments published in:

> Guerra-Urzola, R. et al. (2025). **Optimality Conditions for Penalized Sparse PCA**. *Optimization and Engineering*. [DOI / Link to paper]

The code implements alternating optimization for Sparse PCA under multiple penalties ($L_1$, $L_0$, and SCAD), evaluating convergence speed, variance preservation, and reconstruction sparsity trade-offs.

---

## Repository Structure

```text
├── code/
│   ├── datageneration.R              # Synthetic dataset generators
│   ├── spca_estimations.R            # Core alternating optimization solvers
│   ├── trade-off-curve.R             # Parallelized trade-off simulation
│   ├── permutation_test_experiment.R # Statistical permutation test pipeline
│   ├── data_simulated.R              # Data simulation helpers
│   └── plots.R                       # Figure generation routines
├── figures/                          # Generated PDF figures from experiments
└── README.md
```
## Installation & Prerequisites
This project requires R (>= 4.0.0) and the following CRAN packages:
```r
install.packages(c("dplyr", "ggplot2", "foreach", "doParallel", "progressr"))
```
Clone the repository:
```Bash
git clone https://github.com/RosemberGuerra/OptimalityConditionsSPCA.git
cd OptimalityConditionsSPCA
```
## Reproducing the Experiments
All scripts are configured to be executed from the repository root or the [code/] 
1. Run Core Solvers and SimulationsTo run the alternating optimization benchmarking across penalty types ($L_1$, $L_0$, and SCAD):
```r
# Source core solvers
source("code/spca_estimations.R")

# Run trade-off simulations across parameters 
source("code/trade-off-curve.R")
```
2. Run Permutation Tests
To evaluate statistical significance across variance explained:
```r
source("code/permutation_test_experiment.R")
```
3. Generate Manuscript Figures
Outputs are written directly to the figures/ directory (e.g., tradeOff-var-car.pdf, permutation_test.pdf):
```r
source("code/plots.R")
```
## Core Algorithm Overview
The solver in  [code/spca_estimations.R] implements coordinate descent and alternating optimization:
- alt_spca(): Single sparse component estimation with soft-thresholding ($L_1$), hard-thresholding ($L_0$), or SCAD thresholding.
- alt_spca_multi(): Multivariate extensions using Hotelling/deflation schemes.

## Citation
If you use these algorithms or benchmarks in your research, please cite the manuscript:
```Code snippet
Guerra-Urzola, R., Vera, J.C. & Van Deun, K. Optimality conditions for penalized sparse PCA. Optim Eng 27, 2303–2322 (2026).
```
## License
This repository is distributed under the MIT License - see the [LICENSE] file for details.