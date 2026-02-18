
# Load libraries 
library(dplyr)
library(tidyverse)
library(minpack.lm)

# Load data 
merged_df <- read.csv("~/Downloads/Performance/Project/merged_df.csv")

# Remove the NAs in the df or the exponential decay regression will not work
merged_df <- na.omit(merged_df)

# Define the exponential decay function
exp_decay_model <- function(time, a, b, c) {
  a * exp(-b * time) + c}

# Fit the model
model <- nlsLM(
  HR_recovery_rate ~ a * exp(-b * time) + c,
  data = merged_df,
  start = list(a = max(merged_df$HR_recovery_rate), b = 0.1, c = min(merged_df$HR_recovery_rate)))

# Display model summary
summary(model)

# Compute reasonable initial values
start_a <- max(merged_df$HR_recovery_rate, na.rm = TRUE)
start_c <- min(merged_df$HR_recovery_rate, na.rm = TRUE)
start_b1 <- 1 / (max(merged_df$VO2_Max, na.rm = TRUE) - min(merged_df$VO2_Max, na.rm = TRUE))
start_b2 <- 1 / (max(merged_df$Speed, na.rm = TRUE) - min(merged_df$Speed, na.rm = TRUE))
start_b3 <- 1 / (max(merged_df$Resp_exchange, na.rm = TRUE) - min(merged_df$Resp_exchange, na.rm = TRUE))
start_b4 <- 1 / (max(merged_df$Vent_efficiency, na.rm = TRUE) - min(merged_df$Vent_efficiency, na.rm = TRUE))

model_log <- nlsLM(
  log_HR_recovery ~ log(a * exp(-(b1 * VO2_Max + b2 * Speed + b3 * Resp_exchange + b4 * Vent_efficiency) * time) + c),
  data = merged_df,
  start = list(a = start_a, b1 = start_b1, b2 = start_b2, b3 = start_b3, b4 = start_b4, c = start_c)
)

summary(model_log)

# Re-fit the model with better starting values
model_factors <- nlsLM(
  HR_recovery_rate ~ a * exp(-(b1 * VO2_Max + b2 * Speed + b3 * Resp_exchange + b4 * Vent_efficiency) * time) + c,
  data = merged_df,
  start = list(a = start_a, b1 = start_b1, b2 = start_b2, b3 = start_b3, b4 = start_b4, c = start_c)
)

# Summary of results
summary(model_factors)





