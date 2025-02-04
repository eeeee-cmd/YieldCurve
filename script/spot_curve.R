library(ggplot2)
library(dplyr)
library(purrr)

# Step 1: Load Data
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


# Reshape Data: Convert wide format to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("Jan"), 
               names_to = "Date", 
               values_to = "CleanPrice")

# Convert Date format
df_long$Date <- as.Date(gsub("Jan\\.", "2025-01-", df_long$Date), format="%Y-%m-%d")

# Compute Accrued Interest (assuming semi-annual coupons)
df_long <- df_long %>%
  mutate(
    AccruedInterest = (Time.to.Maturity %% 0.5) / 0.5 * (Coupon / 2), 
    DirtyPrice = CleanPrice + AccruedInterest
  )
# 
# # Bootstrapping Spot Rates
# bootstrap_spot_rates <- function(df) {
#   df <- df %>% arrange(Time.to.Maturity)
#   spot_rates <- numeric(nrow(df))
#   
#   # First bond (zero-coupon) spot rate
#   spot_rates[1] <- -log(df$DirtyPrice[1] / 100) / df$Time.to.Maturity[1]
#   
#   # Iterative Bootstrapping
#   for (i in 2:nrow(df)) {
#     known_cashflows <- sum(
#       (df$Coupon[1:(i-1)] / 2) * exp(-spot_rates[1:(i-1)] * df$Time.to.Maturity[1:(i-1)])
#     )
#     
#     spot_rates[i] <- -log((df$DirtyPrice[i] - known_cashflows) /
#                             (100 + (df$Coupon[i] / 2))) /
#       df$Time.to.Maturity[i]
#   }
#   
#   df$SpotRate <- spot_rates * 100  # Convert to %
#   return(df)
# }
# 
# # Apply Bootstrapping for Each Date
# df_spot_rates <- df_long %>%
#   group_by(Date) %>%
#   group_modify(~ bootstrap_spot_rates(.x))
# 
# # Plot: Superimposed Yield Curve
# ggplot(df_spot_rates, aes(x = Time.to.Maturity, y = SpotRate, color = as.factor(Date))) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   labs(title = "Bootstrapped Yield Curve (Superimposed by Date)", 
#        x = "Maturity (Years)", y = "Spot Rate (%)", color = "Date") +
#   theme_minimal()
# 
# 
# Step 2: Bootstrapping Spot Rates with Semi-Annual Compounding
bootstrap_spot_rates_semiannual <- function(df) {
  df <- df %>% arrange(Time.to.Maturity)
  spot_rates <- numeric(nrow(df))
  
  # First bond (zero-coupon) spot rate
  spot_rates[1] <- -log(df$DirtyPrice[1] / 100) / df$Time.to.Maturity[1]
  
  # Iterative Bootstrapping for semi-annual compounding
  for (i in 2:nrow(df)) {
    # Sum of known discounted cashflows for previous bonds
    known_cashflows <- sum(
      (df$Coupon[1:(i-1)] / 2) * (1 + spot_rates[1:(i-1)] / 200) ^ (-2 * df$Time.to.Maturity[1:(i-1)])
      )
    

# Solve for the spot rate of bond i
spot_rates[i] <- -log((df$DirtyPrice[i] - known_cashflows) / 
                        (100 + (df$Coupon[i] / 2))) / 
  (2 * df$Time.to.Maturity[i])
  }
  
  df$SpotRate <- spot_rates * 100  # Convert to percentage
  return(df)
}

# Apply Bootstrapping for Each Date
df_spot_rates <- df_long %>%
  group_by(Date) %>%
  group_modify(~ bootstrap_spot_rates_semiannual(.x))

# Plot: Superimposed Yield Curve (5-year Spot Rate) with Semi-Annual Compounding
ggplot(df_spot_rates, aes(x = Time.to.Maturity, y = SpotRate, color = as.factor(Date))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Bootstrapped Yield Curve with Semi-Annual Compounding", 
       x = "Maturity (Years)", y = "Spot Rate (%)", color = "Date") +
  theme_minimal()








# Extract the yield columns (excluding the first column, which contains maturity dates)
yield_data <- ytm_results[, -1]

# Convert the data to a matrix
yield_matrix <- as.matrix(yield_data)
# Calculate log-returns
log_returns <- t(apply(yield_matrix, 1, function(x) diff(log(x))))
# Compute the covariance matrix
cov_matrix <- cov(t(log_returns))
print(cov_matrix)

# Select the first 5 maturities
selected_ytm <- ytm_results[1:5, -1] 
# Convert to a matrix
ytm_matrix <- as.matrix(selected_ytm)
# Calculate log-returns
log_returns <- t(apply(ytm_matrix, 1, function(x) diff(log(x))))
# Compute the covariance matrix
cov_matrix <- cov(t(log_returns))
# Print the covariance matrix
print(cov_matrix)








