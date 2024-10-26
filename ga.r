library(GA)
library(nnet)
source("synthetic_data_generation.r")

n_train <- 100  
n_val <- 1000   
n_test <- 1000
noise_level <- 0.2
hardness <- 0.2
min_size <- 1
max_size <- 1
pcrossover <- 0.8
pmutation <- 0.1
popSize <- 50
maxiter <- 100

n_params <- 5 * max_size + (max_size + 1) * 3

# Generate synthetic data
data <- generate_synthetic_data(n_train, n_val, n_test, noise_level, hardness)
train_data <- data$train
val_data <- data$val

# Define the fitness function to set weights and calculate misclassification error
fitness_function <- function(weights) {
    # Instantiate the neural network with the number of neurons given by the first gene
    size <- round(weights[1])
    nn_model <- nnet(
        y_train ~ ., 
        data = train_data, 
        size = size,
        linout = FALSE,
        maxit = 0,
        decay = 0,
        trace = FALSE
    )

    # Set weights of the neural network
    nn_model$wts <- weights[-1]
    
    # Predict on training data
    predictions <- predict(nn_model, as.matrix(train_data[, -ncol(train_data)]), type = "class")
    
    # Calculate misclassification error
    error <- mean(predictions != train_data[, ncol(train_data)])
    
    # Return negative error to maximize fitness function
    return(-error)
}

# Set lower and upper bounds for the weights
lower <- c(min_size, rep(-1, n_params))
upper <- c(max_size, rep(1, n_params))

# Set up GA parameters with filled lower and upper bounds
ga_model <- ga(
    type = "real-valued",
    fitness = fitness_function,
    lower = lower,
    upper = upper,
    pcrossover = pcrossover,
    pmutation = pmutation,
    popSize = popSize,
    maxiter = maxiter,
    run = 50
)

# Retrieve optimized weights
# Returns all of the individuals that are tied for the best fitness score, we only keep the first
optimal_weights <- ga_model@solution[1, ]
optimal_accuracy <- 1 + ga_model@fitnessValue
optimal_size <- round(optimal_weights[1])

nn_model <- nnet(
    y_train ~ .,
    data = train_data,
    size = optimal_size,
    linout = FALSE,
    maxit = 0,
    decay = 0,
    trace = FALSE
)

optimal_n_params <- 5 * optimal_size + (optimal_size + 1) * 3

# Set weights of the neural network
nn_model$wts <- optimal_weights[2:(optimal_n_params + 1)]

# Print results
cat("Optimal Weights Found by GA:", optimal_weights, "\n")
cat("Number of Neurons of Optimal Weights Found by GA:", optimal_size, "\n")
cat("Accuracy of Optimal Weights Found by GA:", optimal_accuracy, "\n")
cat("Weights of Optimal Neural Network Found by GA:", nn_model$wts, "\n")

