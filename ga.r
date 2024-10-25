library(GA)
source("synthetic_data_generation.r")

# Generate synthetic data
data <- generate_synthetic_data(n_train = 100, n_val = 30, n_test = 30, noise_level = 0.1, hardness = 0.5)
train_data <- data$train
val_data <- data$val

# Define fitness function to minimize misclassification error
fitness_function <- function(weights) {
    # Reshape weights into a 4x3 matrix, one column per class
    weights_matrix <- matrix(weights, nrow = 4, ncol = 3)
    
    # Convert feature columns to a numeric matrix
    feature_matrix <- as.matrix(train_data[, -ncol(train_data)])
    
    # Calculate scores for each class
    scores <- feature_matrix %*% weights_matrix  # Result: matrix with 3 columns (one per class)
    
    # Get predictions by finding the highest scoring class for each row
    predictions <- apply(scores, 1, which.max) - 1
    
    # Calculate misclassification error
    error <- mean(predictions != train_data[, ncol(train_data)])
    
    # Return negative error to maximize fitness function
    return(-error)
}

# Set lower and upper bounds for the weights (4x3 = 12 values)
lower <- rep(-1, 12)
upper <- rep(1, 12)

# Set up GA parameters with filled lower and upper bounds
ga_model <- ga(
    type = "real-valued",
    fitness = fitness_function,
    lower = lower,
    upper = upper,
    popSize = 50,
    maxiter = 100,
    run = 50
)

# Retrieve optimized weights
optimal_weights <- ga_model@solution

# Print results
cat("Optimal Weights Found by GA:", optimal_weights, "\n")
