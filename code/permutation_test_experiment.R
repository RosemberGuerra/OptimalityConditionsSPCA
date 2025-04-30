############################################################
# Permutation Test for Comparing Sparse PCA Penalties      #
# Author: RI Guerra Urzola                                 #
# Date: 11-12-2024                                         #
############################################################

# Load required libraries
library(dplyr)  # Data manipulation
library(ggplot2) # Data visualization

# Clear the workspace to avoid conflicts
rm(list = ls())

# Load external scripts and set seed for reproducibility
source("spca_estimations.R")
set.seed(123)

############################################
# Load Simulation Results                  #
############################################
load('results1000.RData')  # Load previously saved results

# Inspect the first few rows of the results
head(results)

############################################
# Filter Results for Relevant Data         #
############################################
# Exclude cases where alpha = 0 or cardinality is at the extreme bounds
results_filter <- results %>% filter(alpha != 0 & !card %in% c(20, 100, 200))

# Extract unique cardinality values and count
u_card <- unique(results_filter$card)
length(u_card)

# Initialize a data frame to store permutation test results
results_test <- data.frame()


#### Perform Permutation Tests ####

# Set the number of permutations
Npermt <- 1000

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = length(u_card), style = 3)
counter <- 0  # Track progress

# Loop through unique cardinality values and penalty levels
for (i in u_card) {
  counter <- counter + 1
  setTxtProgressBar(pb, counter)  # Update progress bar
  
  for (j in levels(results_filter$p)) {
    # Extract data for the current settings and penalties
    group_l0 <- results_filter %>% filter(card == i & p == j & penalty == "L0") %>% select(pev_adj)
    group_l1 <- results_filter %>% filter(card == i & p == j & penalty == "L1") %>% select(pev_adj)
    group_scad <- results_filter %>% filter(card == i & p == j & penalty == "SCAD") %>% select(pev_adj)
    
    # Permutation test: L1 vs L0
    if (length(group_l0[[1]]) < 10 | length(group_l1[[1]]) < 10) {
      ptest <- NA
    } else {
      ptest <- permutation_test(group_l1[[1]], group_l0[[1]], 
                                n_permutations = Npermt, alternative = "greater")
      results_test <- rbind(results_test, data.frame(p = j, card = i,
                                                     p_val = ptest$p_value, testing = "L1 > L0"))
    }
    
    # Permutation test: SCAD vs L0
    if (length(group_l0[[1]]) < 10 | length(group_scad[[1]]) < 10) {
      ptest <- NA
    } else {
      ptest <- permutation_test(group_scad[[1]], group_l0[[1]], 
                                n_permutations = Npermt, alternative = "greater")
      results_test <- rbind(results_test, data.frame(p = j, card = i,
                                                     p_val = ptest$p_value, testing = "SCAD > L0"))
    }
    
    # Permutation test: L1 vs SCAD
    if (length(group_l1[[1]]) < 10 | length(group_scad[[1]]) < 10) {
      ptest <- NA
    } else {
      ptest <- permutation_test(group_l1[[1]], group_scad[[1]], 
                                n_permutations = Npermt, alternative = "greater")
      results_test <- rbind(results_test, data.frame(p = j, card = i,
                                                     p_val = ptest$p_value, testing = "L1 > SCAD"))
    }
  }
}

# Close progress bar
close(pb)


####Visualize Permutation Test Results ####

# Convert columns to factors for plotting
results_test$testing <- as.factor(results_test$testing)
results_test$p <- factor(as.factor(results_test$p), levels = c('p = 20', 'p = 100', 'p = 200'))

# save the results #
save(results_test, file =  paste0('results_pt_',Npermt,'RData'))

# Plot permutation test p-values
ggplot(results_test, aes(x = card, y = p_val)) +
  geom_point(aes(color = p_val > 0.05, alpha = p_val > 0.05), size = 2) +
  scale_color_manual(values = c("black", "gray60")) +  # Black for ≤ 0.05, gray for > 0.05
  scale_alpha_manual(values = c(1, 0.8)) +            # Adjust opacity for significant points
  geom_hline(yintercept = 0.05, color = "black", linetype = "dashed") +  # Significance threshold
  facet_grid(testing ~ p, scales = "free_x") +
  labs(x = 'Number of Features', y = 'Permutation Test p-Value') +
  theme_minimal() +
  theme(legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14)
        )  # Clean up the legend

# Save the plot as a PDF
ggsave("../figures/permutation_test.pdf", width = 10, height = 6, units = "in")

