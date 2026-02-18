# Load libraries
library(dplyr)
library(tidyverse)
library(minpack.lm)

# Load data
merged_df <- read.csv("merged_df.csv")

# Remove NAs
merged_df <- na.omit(merged_df)

# Ensure required columns exist
required_columns <- c("HR_recovery_rate", "VO2_Max", "Speed", "Resp_exchange", "Vent_efficiency", "time")
missing_columns <- setdiff(required_columns, colnames(merged_df))

if (length(missing_columns) > 0) {
  stop(paste("Error: Missing columns in dataset:", paste(missing_columns, collapse = ", ")))
}

# Ensure 'time' column is positive
if (any(merged_df$time <= 0)) {
  stop("Error: 'time' column must contain positive values.")
}

# Add 1 to time
merged_df$time <- merged_df$time

# Normalize predictors to prevent numerical instability
merged_df <- merged_df %>%
  mutate(across(c(VO2_Max, Speed, Resp_exchange, Vent_efficiency, time), scale))

# Compute safe starting values
safe_div <- function(x) {
  range_x <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  ifelse(range_x == 0, 1, 1 / range_x)
}

start_a <- max(merged_df$HR_recovery_rate, na.rm = TRUE)
start_c <- min(merged_df$HR_recovery_rate, na.rm = TRUE)

# Ensure c is positive to avoid log issues
start_c <- max(start_c, 1e-4)  # Modified to avoid log of zero

start_b1 <- safe_div(merged_df$VO2_Max)
start_b2 <- safe_div(merged_df$Speed)
start_b3 <- safe_div(merged_df$Resp_exchange)
start_b4 <- safe_div(merged_df$Vent_efficiency)

# Create log-transformed HR recovery column safely
merged_df <- merged_df %>%
  mutate(log_HR_recovery = log1p(HR_recovery_rate))  # Modified: log() replaced with log1p() to prevent log(0)

# Ensure no NA values were introduced
if (any(is.na(merged_df$log_HR_recovery))) {
  stop("Error: Log transformation resulted in NA values.")
}

# Set parameter bounds to improve numerical stability
lower_bounds <- c(a = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0, c = 0)
upper_bounds <- c(a = Inf, b1 = 1, b2 = 1, b3 = 1, b4 = 1, c = Inf)

# Try fitting the log model, catch errors
tryCatch({
  model_log <- nlsLM(
    log_HR_recovery ~ log1p(a * exp(-(b1 * VO2_Max + b2 * Speed + b3 * Resp_exchange + b4 * Vent_efficiency) * time) + c),
    data = merged_df,
    start = list(a = start_a, b1 = start_b1, b2 = start_b2, b3 = start_b3, b4 = start_b4, c = start_c),
    lower = lower_bounds,
    upper = upper_bounds,
    trace = TRUE,
    control = nls.lm.control(maxiter = 500)
  )
  
  print(summary(model_log))
}, error = function(e) {
  cat("Model failed to converge. Error:", e$message, "\n")
})



# Re-fit the model with better starting values
model_factors <- nlsLM(
  HR_recovery_rate ~ a * exp(-(b1 * VO2_Max + b2 * Speed + b3 * Resp_exchange + b4 * Vent_efficiency) * time) + c,
  data = merged_df,
  start = list(a = start_a, b1 = start_b1, b2 = start_b2, b3 = start_b3, b4 = start_b4, c = start_c)
)

# Summary of results
summary(model_factors)




