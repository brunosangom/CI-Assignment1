# Required Libraries
library(MASS)  # for multivariate normal data generation
library(ggplot2)
library(caret) # for splitting the data

# Function to generate synthetic data
generate_synthetic_data <- function(n_train, n_val, n_test, noise_level = 0.1, hardness = 0.5) {
  # Set seed for reproducibility
  set.seed(123)
  
  # Mean vectors for two classes
  mu_class1 <- c(0, 0)
  mu_class2 <- c(hardness, hardness)  # The hardness parameter controls class separation
  
  # Covariance matrix (identity for simplicity, noise can be added)
  sigma <- diag(2)
  
  # Generate data for class 1
  X_class1_train <- mvrnorm(n_train / 2, mu_class1, sigma)
  X_class1_val <- mvrnorm(n_val / 2, mu_class1, sigma)
  X_class1_test <- mvrnorm(n_test / 2, mu_class1, sigma)
  
  # Generate data for class 2
  X_class2_train <- mvrnorm(n_train / 2, mu_class2, sigma)
  X_class2_val <- mvrnorm(n_val / 2, mu_class2, sigma)
  X_class2_test <- mvrnorm(n_test / 2, mu_class2, sigma)
  
  # Combine the data
  X_train <- rbind(X_class1_train, X_class2_train)
  y_train <- c(rep(0, n_train / 2), rep(1, n_train / 2))
  
  X_val <- rbind(X_class1_val, X_class2_val)
  y_val <- c(rep(0, n_val / 2), rep(1, n_val / 2))
  
  X_test <- rbind(X_class1_test, X_class2_test)
  y_test <- c(rep(0, n_test / 2), rep(1, n_test / 2))
  
  # Add noise to the features (controlled by noise_level)
  X_train <- X_train + rnorm(length(X_train), 0, noise_level)
  X_val <- X_val + rnorm(length(X_val), 0, noise_level)
  X_test <- X_test + rnorm(length(X_test), 0, noise_level)
  
  # Return a list containing the datasets
  list(
    train = data.frame(X_train, y_train = as.factor(y_train)),
    val = data.frame(X_val, y_val = as.factor(y_val)),
    test = data.frame(X_test, y_test = as.factor(y_test))
  )
}

# Example usage:
n_train <- 100  # Vary this for the training set
n_val <- 1000   # Keep validation and test set constant
n_test <- 1000

# Control the amount of noise and problem hardness
noise_level <- 0.3
hardness <- 0.7  # Lower value means harder problem (closer classes)

synthetic_data <- generate_synthetic_data(n_train, n_val, n_test, noise_level, hardness)

# Check the generated data
str(synthetic_data)

# Plot the training data
ggplot(synthetic_data$train, aes(X_train.1, X_train.2, color = y_train)) + 
  geom_point() + 
  theme_minimal() + 
  ggtitle("Synthetic Training Data")
