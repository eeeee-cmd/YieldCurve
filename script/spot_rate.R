# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Load dataset
df <- read.csv("data/analysis_data/analysis_data2.csv")
# Convert closing prices to numeric by removing dollar signs
price_columns <- names(df)[6:length(df)]
df[price_columns] <- lapply(df[price_columns], function(x) as.numeric(gsub("[$,]", "", x)))

# Convert dates
df$Maturity.Date <- as.Date(df$Maturity.Date, format="%Y-%m-%d")
df$Issue.Date <- as.Date(df$Issue.Date, format="%Y-%m-%d")

# Convert coupon rate to decimal
df$Coupon <- as.numeric(gsub("%", "", df$Coupon)) / 100

# Convert Maturity Date to Date format and calculate Time to Maturity
df <- df %>%
  mutate(Time.to.Maturity = as.numeric(difftime(Maturity.Date, as.Date("2025-01-17"), units = "days")) / 365)

df_long <- df %>%
  pivot_longer(cols = starts_with("Jan"), 
               names_to = "Date", 
               values_to = "Price") %>%
  mutate(Date = gsub(".Close.Price", "", Date),  # Remove unnecessary text
         Date = as.Date(paste0("2025-01-", gsub("Jan.", "", Date)), format = "%Y-%m-%d")) %>% 
  rename(Coupon = Coupon, 
         Maturity = Time.to.Maturity) %>%
  arrange(Date, Maturity)  # Sort by date and maturity


# Function to compute spot rates using bootstrapping
bootstrap_spot_curve <- function(bond_data) {
  bond_data <- bond_data %>% arrange(Maturity)  # Sort bonds by maturity
  spot_rates <- numeric(nrow(bond_data))
  
  for (i in 1:nrow(bond_data)) {
    P <- bond_data$Price[i]  # Bond price
    C <- bond_data$Coupon[i] / 2  # Semi-annual coupon payment
    F <- 100  # Face value
    n <- bond_data$Maturity[i]  # Years to maturity
    
    if (C == 0) {
      # Zero-coupon bond formula
      spot_rates[i] <- (F / P)^(1/n) - 1
    } else {
      # Solve iteratively using previous spot rates
      sum_previous <- sum(C / (1 + spot_rates[1:(i-1)])^(1:(i-1)))
      spot_rates[i] <- ((P - sum_previous) / (C + F))^(1/n) - 1
    }
  }
  
  bond_data$Spot_Rate <- spot_rates
  return(bond_data)
}

# Apply the bootstrapping function to each date
spot_curve_data <- df_long %>%
  group_by(Date) %>%
  group_modify(~ bootstrap_spot_curve(.))

# Convert Date to formatted string for legend
spot_curve_data$Date_Label <- format(spot_curve_data$Date, "%b.%d")

# Plot the Spot Rate Curves with formatted legend
ggplot(spot_curve_data, aes(x = Maturity, y = Spot_Rate, color = as.factor(Date_Label), group = Date_Label)) +
  geom_line() +
  labs(title = "Spot Rate Curves",
       x = "Maturity (Years)", y = "Spot Rate",
       color = "Date") +
  theme_minimal()









# Initialize a list to store forward curves for each date
forward_curves <- list()

# Loop over each date to calculate the forward rates
for (date in unique(spot_curve_data$Date)) {
  
  # Filter the spot rates for the current date
  spot_rates <- spot_curve_data %>%
    filter(Date == date) %>%
    arrange(Maturity) %>%
    pull(Spot_Rate)  # Extract the spot rates
  
  forward_rates <- c()  # Initialize a vector to store forward rates
  
  # Compute forward rates for maturities 2 to 5 years
  for (n in 2:5) {
    if (n <= length(spot_rates)) {
      r_n <- spot_rates[n]  # Spot rate for maturity n
      r_1 <- spot_rates[1]  # 1-year spot rate
      
      # Forward rate formula
      f_1n <- 2 * (( (1 + r_n / 2)^(n * 2) / (1 + r_1 / 2)^2 )^(1 / ((n - 1) * 2)) - 1)
      forward_rates <- c(forward_rates, f_1n)
    }
  }
  # Store the forward rates for the current date
  forward_curves[[as.character(date)]] <- forward_rates
}

date_labels = c("Jan. 06", "Jan. 07", "Jan. 08", "Jan. 09", "Jan. 10", "Jan. 13", "Jan. 14", "Jan. 15", "Jan. 16", "Jan. 17")

# Convert the forward curves into a data frame for plotting
forward_curve_df <- data.frame(
  Maturity = rep(2:5, times = length(forward_curves)),
  Forward_Rate = unlist(forward_curves),
  Date = rep(date_labels, each = 4)
)

# Convert the Date column to a factor and ensure consistent date formatting for the legend
forward_curve_df$Date <- factor(forward_curve_df$Date, levels = unique(forward_curve_df$Date))

# Plot the forward curves using ggplot2
ggplot(forward_curve_df, aes(x = Maturity, y = Forward_Rate, color = Date, group = Date)) +
  geom_line() +
  geom_point() +
  labs(title = "1-Year Forward Curve for Each Day",
       x = "Years to Maturity (Starting at 1yr)",
       y = "Forward Rate",
       color = "Date") +
  theme_minimal()






# Load necessary libraries
library(knitr)

# Extract YTM values (excluding the first column, which contains maturity dates)
ytm_values <- ytm_results[, -1]

# Convert to a matrix
ytm_matrix <- as.matrix(ytm_values)

# Group into 5-year buckets (assuming rows are ordered by maturity)
ytm_buckets <- ytm_matrix[1:5, ]

# Calculate log-returns for yields
log_returns_yields <- t(apply(ytm_buckets, 1, function(x) diff(log(x))))
log_returns_yields <- t(log_returns_yields)

# Add column names for yields
colnames(log_returns_yields) <- paste0("Yield_", 1:5, "yr")

# Add row names for yields
rownames(log_returns_yields) <- paste0("Day_", 1:nrow(log_returns_yields))

# Function to calculate forward rates
calculate_forward_rates <- function(ytm_buckets) {
  n <- nrow(ytm_buckets)
  forward_rates <- matrix(0, nrow = n - 1, ncol = ncol(ytm_buckets))
  
  for (i in 1:(n - 1)) {
    t1 <- i  # t1 = 1yr, 2yr, 3yr, 4yr
    t2 <- i + 1  # t2 = 2yr, 3yr, 4yr, 5yr
    forward_rates[i, ] <- (ytm_buckets[t2, ] * t2 - ytm_buckets[t1, ] * t1) / (t2 - t1)
  }
  
  return(forward_rates)
}

# Calculate forward rates
forward_rates <- calculate_forward_rates(ytm_buckets)

# Calculate log-returns for forward rates
log_returns_forward <- t(apply(forward_rates, 1, function(x) diff(log(x))))
log_returns_forward <- t(log_returns_forward)

# Add column names for forward rates
colnames(log_returns_forward) <- paste0("Forward_", 1:4, "yr")
# Add row names for forward rates
rownames(log_returns_forward) <- paste0("Day_", 1:nrow(log_returns_forward))

# Compute the covariance matrix for yields
cov_matrix_yields <- cov(log_returns_yields)

# Add row and column names for covariance matrix of yields
rownames(cov_matrix_yields) <- paste0("Yield_", 1:5, "yr")
colnames(cov_matrix_yields) <- paste0("Yield_", 1:5, "yr")

# Compute the covariance matrix for forward rates
cov_matrix_forward <- cov(log_returns_forward)

# Add row and column names for covariance matrix of forward rates
rownames(cov_matrix_forward) <- paste0("Forward_", 1:4, "yr")
colnames(cov_matrix_forward) <- paste0("Forward_", 1:4, "yr")

# Print log-return matrix for yields using kable
print("Log-Return Matrix for Yields (10x5):")
kable(log_returns_yields, caption = "Log-Return Matrix for Yields (10x5)")

# Print log-return matrix for forward rates using kable
print("Log-Return Matrix for Forward Rates (10x4):")
kable(log_returns_forward, caption = "Log-Return Matrix for Forward Rates (10x4)")

# Print covariance matrix for yields using kable
print("Covariance Matrix of Yields (5x5):")
kable(cov_matrix_yields, caption = "Covariance Matrix of Yields (5x5)")

# Print covariance matrix for forward rates using kable
print("Covariance Matrix of Forward Rates (4x4):")
kable(cov_matrix_forward, caption = "Covariance Matrix of Forward Rates (4x4)")

