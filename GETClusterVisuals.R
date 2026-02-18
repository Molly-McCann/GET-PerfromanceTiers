library(ggplot2)
library(dplyr)

# Set the folder path containing CSV files
folder_path <- "~/Downloads/Performance/Project/Part1Group2/"

# List all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Function to generate plots for each dataset
generate_plots <- function(file) {
  # Read CSV
  data <- read.csv(file)
  
  # Ensure required columns exist
  if (!all(c("Age", "VO2_Max", "Sex", "Peak_HR", "HR_recovery_rate", 
             "HR_recovery_drop_percent", "Speed_HR_ratio", "VO2_Speed_ratio", 
             "HR_variability", "Humidity", "Temperature") %in% colnames(data))) {
    message(paste("Skipping", file, "due to missing columns"))
    return(NULL)
  }
  
  # Convert Sex to a factor for better visualization
  data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Male", "Female"))
  
  # Scatter Plot: Age vs. VO2_Max
  p1 <- ggplot(data, aes(x = Age, y = VO2_Max, color = Sex)) +
    geom_point() +
    scale_color_manual(values = c("blue", "red")) +
    labs(title = "Age vs. VO2_Max", x = "Age", y = "VO2 Max") +
    theme_minimal()
  print(p1)
  
  # Bubble Chart: Peak HR vs. Recovery Rate
  p2 <- ggplot(data, aes(x = Peak_HR, y = HR_recovery_rate, size = HR_recovery_drop_percent, color = Sex)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = c("blue", "red")) +
    labs(title = "Peak HR vs. Recovery Rate", x = "Peak HR", y = "HR Recovery Rate") +
    theme_minimal()
  print(p2)
  
  # Scatter Plot with Trendline: Speed vs. HR Efficiency
  p3 <- ggplot(data, aes(x = Speed_HR_ratio, y = VO2_Speed_ratio, color = Age)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_color_gradient(low = "blue", high = "red") +
    labs(title = "Speed vs. HR Efficiency", x = "Speed-HR Ratio", y = "VO2-Speed Ratio") +
    theme_minimal()
  print(p3)
  
  # Box Plot: HR Variability by Age Group
  data$Age_Group <- cut(data$Age, breaks = seq(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE), by = 2))
  p4 <- ggplot(data, aes(x = Age_Group, y = HR_variability, fill = Age_Group)) +
    geom_boxplot() +
    labs(title = "HR Variability by Age", x = "Age Group", y = "HR Variability") +
    theme_minimal() +
    theme(legend.position = "none")
  print(p4)
  
  # Heatmap: Environmental Impact on Performance
  p5 <- ggplot(data, aes(x = cut(Temperature, breaks = 3), y = cut(Humidity, breaks = 3), fill = VO2_Max)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Environmental Impact", x = "Temperature", y = "Humidity", fill = "VO2 Max") +
    theme_minimal()
  print(p5)
}

# Loop through each CSV and generate plots
lapply(csv_files, generate_plots)
