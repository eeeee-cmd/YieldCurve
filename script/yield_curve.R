# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

# Load data
# data <- read.csv("data/analysis_data/analysis_data.csv")
data <- read.csv("data/analysis_data/analysis_data2.csv")

# Convert closing prices to numeric by removing dollar signs
price_columns <- names(data)[6:length(data)]
data[price_columns] <- lapply(data[price_columns], function(x) as.numeric(gsub("[$,]", "", x)))

# Convert dates
data$Maturity.Date <- as.Date(data$Maturity.Date, format="%Y-%m-%d")
data$Issue.Date <- as.Date(data$Issue.Date, format="%Y-%m-%d")

# Convert coupon rate to decimal
data$Coupon <- as.numeric(gsub("%", "", data$Coupon)) / 100

# Convert Maturity Date to Date format and calculate Time to Maturity
data <- data %>%
  mutate(Time.to.Maturity = as.numeric(difftime(Maturity.Date, as.Date("2025-01-17"), units = "days")) / 365)

# Function to calculate YTM using numerical solving
ytm_solver <- function(price, face_value, coupon_rate, years_to_maturity, num_coupons = 2) {
  coupon_payment <- (coupon_rate * face_value) / num_coupons
  periods <- years_to_maturity * num_coupons
  
  # Define bond price equation
  bond_price_eq <- function(y) {
    sum(coupon_payment / (1 + y / num_coupons)^(1:periods)) +
      face_value / (1 + y / num_coupons)^periods - price
  }
  
  # Solve for YTM using uniroot
  result <- tryCatch({
    uniroot(bond_price_eq, c(0, 1))$root
  }, error = function(e) { NA })
  
  return(result)
}

# Compute YTM for each bond for each closing price date
face_value <- 100
ytm_results <- data.frame(Maturity.Date = data$Maturity.Date)

for (date in price_columns) {
  ytm_results[[date]] <- mapply(ytm_solver, 
                                price = data[[date]], 
                                face_value = face_value, 
                                coupon_rate = data$Coupon, 
                                years_to_maturity = as.numeric(difftime(data$Maturity.Date, as.Date("2025-01-06"), units="days")) / 365)
}

# Reshape the data for plotting
ytm_long <- ytm_results %>%
  pivot_longer(-Maturity.Date, names_to = "Date", values_to = "YTM") %>%
  mutate(Date = gsub(" Close Price", "", Date),
         Years.to.Maturity = as.numeric(difftime(Maturity.Date, as.Date("2025-01-06"), units="days")) / 365)

# Plot the yield curves
ggplot(ytm_long, aes(x = Years.to.Maturity, y = YTM, color = Date, group = Date)) +
  geom_line(aes(linetype = Date)) +
  geom_point() +
  labs(title = "5-Year Yield Curve Over Time",
       x = "Years to Maturity",
       y = "Yield to Maturity (YTM)") +
  theme_minimal()

# Apply cubic spline interpolation correctly
ytm_spline <- ytm_long %>%
  group_by(Date) %>%
  summarise(
    Years.to.Maturity.Fine = list(seq(min(Years.to.Maturity), max(Years.to.Maturity), length.out = 100)),
    YTM.Fine = list(spline(Years.to.Maturity, YTM, xout = seq(min(Years.to.Maturity), max(Years.to.Maturity), length.out = 100))$y)
  ) %>%
  unnest(cols = c(Years.to.Maturity.Fine, YTM.Fine))

# Plot the yield curves using cubic spline interpolation
ggplot(ytm_spline, aes(x = Years.to.Maturity.Fine, y = YTM.Fine, color = Date, group = Date)) +
  geom_line() +
  labs(title = "5-Year Yield Curve (Cubic Spline Interpolation)",
       x = "Years to Maturity",
       y = "Yield to Maturity (YTM)") +
  theme_minimal()


