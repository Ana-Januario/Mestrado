# ==============================================================================
# Example Analysis Script Template
# ==============================================================================
# Description: Template for data analysis scripts
# Author: [Your Name]
# Date: [Date]
# ==============================================================================

# Load required packages -------------------------------------------------------
# Uncomment and install packages as needed:
# install.packages(c("tidyverse", "ggplot2", "dplyr", "readr"))

library(tidyverse)

# Set working directory (if needed) --------------------------------------------
# setwd("path/to/Mestrado")

# Load data --------------------------------------------------------------------
# Example:
# data_raw <- read.csv("data/raw/your_dataset.csv")

# Data exploration -------------------------------------------------------------
# str(data_raw)
# summary(data_raw)
# head(data_raw)

# Data cleaning and preprocessing ----------------------------------------------
# data_clean <- data_raw %>%
#   filter(!is.na(variable)) %>%
#   mutate(new_variable = ...)

# Exploratory analysis ---------------------------------------------------------
# Create summary statistics
# summary_stats <- data_clean %>%
#   group_by(category) %>%
#   summarise(
#     mean_value = mean(value),
#     sd_value = sd(value),
#     n = n()
#   )

# Visualization ----------------------------------------------------------------
# Create plots
# plot1 <- ggplot(data_clean, aes(x = x_var, y = y_var)) +
#   geom_point() +
#   theme_minimal() +
#   labs(title = "Your Title", x = "X Label", y = "Y Label")

# Save plot
# ggsave("figures/example_plot.png", plot1, width = 8, height = 6, dpi = 300)

# Statistical modeling ---------------------------------------------------------
# Example: Linear regression
# model1 <- lm(y ~ x1 + x2, data = data_clean)
# summary(model1)

# Save model
# saveRDS(model1, "models/model1.rds")

# Export processed data --------------------------------------------------------
# write.csv(data_clean, "data/processed/data_clean.csv", row.names = FALSE)

# Session information ----------------------------------------------------------
sessionInfo()
