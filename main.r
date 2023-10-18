# Loading necessary libraries
library(ggplot2)

# Fetching data
data <- airquality

# Data Cleaning - Remove rows with NA
data <- data[!is.na(data$Ozone) & !is.na(data$Solar.R) & !is.na(data$Wind) & !is.na(data$Temp) & !is.na(data$Month) & !is.na(data$Day), ]

# Function to calculate monthly average solar radiation
calculate_monthly_avg <- function(data) {
  data %>%
    group_by(Month) %>%
    summarize(avg_solar = mean(Solar.R, na.rm = TRUE))
}

# Call the function and store the result
monthly_averages <- calculate_monthly_avg(data)
print(monthly_averages)

# Add a column with month names
monthly_averages <- monthly_averages %>%
  mutate(month_name = paste("avg_solar_", month.name[Month], sep = ""))
print(monthly_averages)

# Calculate and print average solar radiation for each month
monthly_averages$month_name <- month.name[monthly_averages$Month]

apply(monthly_averages, 1, function(row) {
  cat("Average Solar Radiation for", row["month_name"], ": ", row["avg_solar"], "\n\n")
})

# Analysis Part 2: Correlation between Ozone and Solar Radiation for each month
correlation_by_month <- function(month, data) {
  cor(data$Ozone[data$Month == month], data$Solar.R[data$Month == month])
}

# Print correlations for each month
months <- 5:9
correlations <- sapply(months, function(month) {
  correlation <- correlation_by_month(month, data)
  print(paste("Correlation for Month", month, ":", correlation))
  return(correlation)
})

# Visualization
plot_shapes <- vector("character", length = nrow(data))
for (i in 1:nrow(data)) {
  if (correlations[1] > 0.5) {
    plot_shapes[i] <- 19
  } else if (correlations[2] > 0.5) {
    plot_shapes[i] <- 17
  } else {
    plot_shapes[i] <- 15
  }
}

# Save Plots
save_plot <- function(month, filename) {
  g <- ggplot(data[data$Month == month, ], aes(x = Solar.R, y = Ozone)) + geom_point(aes(shape = factor(Month))) + ggtitle(month.name[month])
  ggsave(filename, g)
}

# Save plots for each month
months <- 5:9
filenames <- paste("plot_", tolower(month.name[months]), ".png", sep = "")
Map(save_plot, months, filenames)

# Save data
write.csv(data, "cleaned_data.csv", row.names = FALSE)
