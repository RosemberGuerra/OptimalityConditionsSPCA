library(dplyr)
library(ggplot2)

# 
load('results1000.RData')

# Step 1: Calculate mean PEV per card
mean_pev <- results %>%
  group_by(penalty, n, p, card) %>%
  summarize(mean_pev = mean(pev / 100), .groups = 'drop')

# Step 2: Select 5 evenly spaced points in each group
highlight_data <- mean_pev %>%
  group_by(penalty, n, p) %>%
  arrange(card) %>%
  slice(round(seq(1, n(), length.out = 5))) %>%
  ungroup()

# Step 3: Plot lines with selected points only 
ggplot(mean_pev, aes(x = card, y = mean_pev,
                      linetype = penalty, color = penalty)) +
  geom_line(linewidth = 1) +
  geom_point(data = highlight_data, aes(shape = penalty), size = 3) +
  scale_color_manual(
    values = c("L1" = "black", "L0" = "gray30", "SCAD" = "gray60")
  ) +
  scale_linetype_manual(
    values = c("L1" = "dotted", "L0" = "dashed", "SCAD" = "solid")
  ) +
  scale_shape_manual(
    values = c("L1" = 16, "L0" = 17, "SCAD" = 15)
  ) +
  facet_grid(n ~ p, scales = "free_x") +
  labs(
    x = 'Number of features',
    y = 'Mean Proportion of Variance Explained',
    color = 'Penalty',
    linetype = 'Penalty',
    shape = 'Penalty'
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# save plot as pdf
ggsave("../figures/tradeOff-var-car.pdf", 
       width = 10, height = 6, units = "in", device = cairo_pdf)

#### adjusted variance ####

# Step 1: Calculate mean PEV per card
mean_pev_adj <- results %>%
  group_by(penalty, n, p, card) %>%
  summarize(mean_pev_adj = mean(pev_adj / 100), .groups = 'drop')

# Step 2: Select 5 evenly spaced points in each group
highlight_pev_adj <- mean_pev_adj %>%
  group_by(penalty, n, p) %>%
  arrange(card) %>%
  slice(round(seq(1, n(), length.out = 5))) %>%
  ungroup()

# Step 3: Plot lines with selected points only #
ggplot(mean_pev_adj, aes(x = card, y = mean_pev_adj,
                         linetype = penalty, color = penalty)) +
  geom_line(linewidth = 1) +
  geom_point(data = highlight_pev_adj, aes(shape = penalty), size = 3) +
  scale_color_manual(
    values = c("L1" = "black", "L0" = "gray30", "SCAD" = "gray60")
  ) +
  scale_linetype_manual(
    values = c("L1" = "dotted", "L0" = "dashed", "SCAD" = "solid")
  ) +
  scale_shape_manual(
    values = c("L1" = 16, "L0" = 17, "SCAD" = 15)
  ) +
  facet_grid(n ~ p, scales = "free_x") +
  labs(
    x = 'Number of features',
    y = 'Mean Proportion of Variance Explained',
    color = 'Penalty',
    linetype = 'Penalty',
    shape = 'Penalty'
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# save plot as pdf
ggsave("../figures/tradeOff-var_adj-car.pdf", 
       width = 10, height = 6, units = "in", device = cairo_pdf)

#### adjusted variance ####

# Step 1: Calculate mean PEV per card
mean_pev_adj <- results %>%
  group_by(penalty, n, p, card) %>%
  summarize(mean_pev_adj = mean(pev_adj / 100), .groups = 'drop')

# Step 2: Select 5 evenly spaced points in each group
highlight_pev_adj <- mean_pev_adj %>%
  group_by(penalty, n, p) %>%
  arrange(card) %>%
  slice(round(seq(1, n(), length.out = 5))) %>%
  ungroup()

# Step 3: Plot lines with selected points only #
ggplot(mean_pev_adj, aes(x = card, y = mean_pev_adj,
                         linetype = penalty, color = penalty)) +
  geom_line(linewidth = 1) +
  geom_point(data = highlight_pev_adj, aes(shape = penalty), size = 3) +
  scale_color_manual(
    values = c("L1" = "black", "L0" = "gray30", "SCAD" = "gray60")
  ) +
  scale_linetype_manual(
    values = c("L1" = "dotted", "L0" = "dashed", "SCAD" = "solid")
  ) +
  scale_shape_manual(
    values = c("L1" = 16, "L0" = 17, "SCAD" = 15)
  ) +
  facet_grid(n ~ p, scales = "free_x") +
  labs(
    x = 'Number of features',
    y = 'Mean Proportion of Variance Explained',
    color = 'Penalty',
    linetype = 'Penalty',
    shape = 'Penalty'
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# save plot as pdf
ggsave("../figures/tradeOff-var_adj-car.pdf", 
       width = 10, height = 6, units = "in", device = cairo_pdf)

#### Iterations ####

results_filter = results %>% filter(card != c(20,100,200))

# Step 1: Calculate mean PEV per card
mean_iter <- results_filter %>%
  group_by(penalty, n, p, card) %>%
  summarize(mean_iter = mean(spca.iter), .groups = 'drop')

# Step 2: Select 5 evenly spaced points in each group
highlight_iter <- mean_iter %>%
  group_by(penalty, n, p) %>%
  arrange(card) %>%
  slice(round(seq(1, n(), length.out = 5))) %>%
  ungroup()

# Step 3: Plot lines with selected points only 
ggplot(mean_iter, aes(x = card, y = mean_iter,
                      linetype = penalty, color = penalty)) +
  geom_line(linewidth = 1) +
  geom_point(data = highlight_iter, aes(shape = penalty), size = 3) +
  scale_color_manual(
    values = c("L1" = "black", "L0" = "gray30", "SCAD" = "gray60")
  ) +
  scale_linetype_manual(
    values = c("L1" = "dotted", "L0" = "dashed", "SCAD" = "solid")
  ) +
  scale_shape_manual(
    values = c("L1" = 16, "L0" = 17, "SCAD" = 15)
  ) +
  facet_grid(n ~ p, scales = "free_x") +
  labs(
    x = 'Number of features',
    y = 'Average Number of Iterations',
    color = 'Penalty',
    linetype = 'Penalty',
    shape = 'Penalty'
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# save plot as pdf
ggsave("../figures/tradeOff-iter-car.pdf", 
       width = 10, height = 6, units = "in", device = cairo_pdf)

#### Time ####


results_filter_time = results %>% filter(spca.time < 1 & card != c(20,100,200))

# Step 1: Calculate mean PEV per card
mean_time <- results_filter_time %>%
  group_by(penalty, n, p, card) %>%
  summarize(mean_time = mean(spca.time), .groups = 'drop')

# Step 2: Select 5 evenly spaced points in each group
highlight_time <- mean_time %>%
  group_by(penalty, n, p) %>%
  arrange(card) %>%
  slice(round(seq(1, n(), length.out = 5))) %>%
  ungroup()

# Step 3: Plot lines with selected points only 
ggplot(mean_time, aes(x = card, y = mean_time,
                      linetype = penalty, color = penalty)) +
  geom_line(linewidth = 1) +
  geom_point(data = highlight_time, aes(shape = penalty), size = 3) +
  scale_color_manual(
    values = c("L1" = "black", "L0" = "gray30", "SCAD" = "gray60")
  ) +
  scale_linetype_manual(
    values = c("L1" = "dotted", "L0" = "dashed", "SCAD" = "solid")
  ) +
  scale_shape_manual(
    values = c("L1" = 16, "L0" = 17, "SCAD" = 15)
  ) +
  facet_grid(n ~ p, scales = "free_x") +
  labs(
    x = 'Number of features',
    y = 'Average Time (sec)',
    color = 'Penalty',
    linetype = 'Penalty',
    shape = 'Penalty'
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# save plot as pdf
ggsave("../figures/tradeOff-time-car.pdf", 
       width = 10, height = 6, units = "in", device = cairo_pdf)

