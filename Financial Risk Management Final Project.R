?????????????????????????????????(1.1)
# Define the annual default probabilities for each rating
default_probs <- c(0.0025, 0.0040, 0.0065, 0.0100, 0.0160, 0.0250, 0.0400)

# Calculate the default intensities (λ) for each rating
default_intensities <- -log(1 - default_probs)

# Print the default intensities
cat("Default Intensities (λ):", default_intensities)

?????????????????????????????????(1.2)
# Load necessary libraries
library(copula)
library(MASS)

# Define default intensities, copula correlation, and LGD
default_intensities <- c(0.00250313, 0.004008021, 0.006521217, 0.01005034, 0.01612938, 0.02531781, 0.04082199)
rho <- 0.15
LGD <- 0.5

# Number of simulations
n_sims <- 10000

# Generate correlated default times using Gaussian copula
normal_copula <- normalCopula(rho, dim = length(default_intensities))
u <- rCopula(n_sims, normal_copula)
correlated_times <- qexp(u, rate = default_intensities)

# Calculate default losses
losses <- rowSums(correlated_times <= 1) * LGD * 5

# Compute probabilities for each interval
loss_intervals <- seq(0, 95, by = 5)
probabilities <- sapply(loss_intervals, function(l) {
  mean(losses >= l & losses < l + 5)
})

# Output the probabilities along with the losses in each interval
output <- data.frame(Interval = paste0("[", loss_intervals, ", ", loss_intervals + 5, ")"), Probability = probabilities)
print(output)



????????????????????????????(2.1)
# Define the rating categories and their corresponding default probabilities
rating_prob <- c("BBB" = 0.0025, "BBB-" = 0.004, "BB+" = 0.0065, "BB" = 0.01, "BB-" = 0.016, "B+" = 0.025, "B" = 0.04)

# Define the exposure per obligor
exposure <- 10e6

# Calculate the distances to default
dist_to_default <- -qnorm(rating_prob)

# Print the distances to default for each rating
for (rating in names(rating_prob)) {
  cat("Rating:", rating, "Distance to Default:", dist_to_default[rating], "\n")
}


????????????????????????????(2.2)
# Load required libraries
library(MASS)

# Define parameters
n_simulations <- 10000
n_obligors <- 200
exposure <- 10000000
rho_A <- 0.30
LGD <- 0.5
ratings <- c("BBB", "BBB-", "BB+", "BB", "BB-", "B+", "B")
n_ratings <- c(10, 10, 40, 80, 40, 10, 10)
default_prob <- c(0.0025, 0.004, 0.0065, 0.01, 0.016, 0.025, 0.04)
dd <- rep(0, n_obligors)

# Calculate distances to default
for (i in 1:length(ratings)) {
  dd <- c(dd, rep((log(exposure / (1 - LGD)) - log(1 - default_prob[i])) / (sqrt(1 - rho_A) * qnorm(default_prob[i])), n_ratings[i]))
}

# Perform simulations
losses <- rep(0, n_simulations)
for (sim in 1:n_simulations) {
  z <- rnorm(n_obligors)
  correlated_z <- sqrt(rho_A) * rnorm(1) + sqrt(1 - rho_A) * z
  default_indicator <- ifelse(correlated_z < dd, 0, 1)
  losses[sim] <- sum(default_indicator * exposure * LGD)
}

# Calculate probabilities for each interval
interval_prob <- rep(0, 20)
for (i in 0:19) {
  lower_bound <- i * 5000000
  upper_bound <- lower_bound + 5000000
  interval_prob[i + 1] <- sum(losses >= lower_bound & losses < upper_bound) / n_simulations
}

# Print results
cat("Probabilities for each interval:\n")
for (i in 1:length(interval_prob)) {
  cat(sprintf("Interval %d: [%d, %d) - Probability: %.4f\n", i, (i - 1) * 5000000, i * 5000000, interval_prob[i]))
}


????????????????????????????(3.1)
# Install required packages
if (!require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)

# Parameters
ratings <- c("BBB", "BBB-", "BB+", "BB", "BB-", "B+", "B")
default_prob <- c(0.0025, 0.004, 0.0065, 0.01, 0.016, 0.025, 0.04)
asset_values <- c(500, 450, 400, 350, 300, 250, 200) # in millions
asset_volatility <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
debt_face_values <- c(150, 160, 180, 200, 220, 240, 260) # in millions
num_obligors <- c(10, 10, 40, 80, 40, 10, 10)

# Calculate distance to default using Black-Scholes Merton model
distance_to_default <- function(asset_value, debt_face_value, asset_volatility) {
  d1 <- (log(asset_value / debt_face_value)) / (asset_volatility * sqrt(1)) + 0.5 * asset_volatility * sqrt(1)
  return(d1)
}

# Calculate distance to default for each rating
df <- data.frame(Rating = ratings,
                 AssetValue = asset_values,
                 DebtFaceValue = debt_face_values,
                 AssetVolatility = asset_volatility) %>%
  mutate(DistanceToDefault = distance_to_default(AssetValue, DebtFaceValue, AssetVolatility))

# Output
print(df[, c("Rating", "DistanceToDefault")])


????????????????????????????(3.2)
library(MASS)

# Parameters
num_obligors <- 200
exposure <- 10000000
total_exposure <- num_obligors * exposure
rho_A <- 0.3
LGD <- 0.5
time_step <- 1 / 52

# Ratings
ratings <- c("BBB", "BBB-", "BB+", "BB", "BB-", "B+", "B")
num_ratings <- c(10, 10, 40, 80, 40, 10, 10)

# Annual Default Probability
default_probs <- c(0.0025, 0.0040, 0.0065, 0.0100, 0.0160, 0.0250, 0.0400)

# Distance to Default
distance_to_default <- c(12.0897280, 6.9688251, 4.0925385, 2.3634632, 1.1838498, 0.2916343, -0.4559107)

# Create a dataframe for obligors
obligors <- data.frame()
for (i in 1:length(ratings)) {
  obligors <- rbind(obligors, data.frame(rating = ratings[i], num = num_ratings[i], prob = default_probs[i], dtd = distance_to_default[i]))
}
obligors$exposure <- exposure

# Simulate defaults
set.seed(123)
num_simulations <- 10000
losses <- numeric(num_simulations)

for (sim in 1:num_simulations) {
  z <- rnorm(num_obligors)
  common_factor <- sqrt(rho_A) * rnorm(1)
  
  for (i in 1:num_obligors) {
    obligor <- obligors[i,]
    epsilon <- sqrt(1 - rho_A) * z[i]
    
    # Calculate asset return
    asset_return <- common_factor + epsilon
    
    # Check if distance-to-default reaches zero
    dtd_updated <- obligor$dtd + asset_return * time_step
    if (dtd_updated <= 0) {
      losses[sim] <- losses[sim] + obligor$exposure * LGD
    }
  }
}

# Calculate loss probabilities
loss_intervals <- seq(0, 95000000, 5000000)
prob_loss_intervals <- numeric(length(loss_intervals) - 1)

for (i in 1:(length(loss_intervals) - 1)) {
  prob_loss_intervals[i] <- sum(losses >= loss_intervals[i] & losses < loss_intervals[i + 1]) / num_simulations
}

# Output results
results <- data.frame(Interval_Start = loss_intervals[-length(loss_intervals)],
                      Interval_End = loss_intervals[-1] - 1,
                      Probability = prob_loss_intervals)
print(results)
