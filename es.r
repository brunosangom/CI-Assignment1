# Load necessary libraries
library(nnet)
library(cmaesr)
source("synthetic_data_generation.r")

n_train <- 100  
n_val <- 1000   
n_test <- 1000
noise_level <- 0.2
hardness <- 0.2
min_size <- 1
max_size <- 1
sigma <- 0.5
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

# Set initial mean and bounds for the weights
lower <- c(min_size, rep(-1, n_params))
upper <- c(max_size+1, rep(1, n_params))

obj.fn = makeSingleObjectiveFunction(
  name = "Neural Network Fitness Function",
  fn = function(x) -fitness_function(x),
  par.set = makeNumericParamSet("weights", len = n_params + 1 , lower = lower, upper = upper)
)

# Initialize the ES model
es_model <- cmaes(
    obj.fn, 
    monitor = makeMonitor(),
    control = list(
        sigma = sigma, # initial step size
        lambda = popSize, # number of offspring
        stop.ons = c(
            list(stopOnMaxIters(maxiter)), # stop after maxiter iterations
            getDefaultStoppingConditions() # or after default stopping conditions
        )
    )
)

# Retrieve optimized weights and accuracy
optimal_weights <- es_model$best.param
optimal_accuracy <- 1 - es_model$best.fitness
optimal_size <- round(optimal_weights[1])

# Print results
cat("Optimal Weights Found by CMA-ES:", optimal_weights, "\n")
cat("Number of Neurons of Optimal Weights Found by CMA-ES:", optimal_size, "\n")
cat("Accuracy of Optimal Weights Found by CMA-ES:", optimal_accuracy, "\n")
