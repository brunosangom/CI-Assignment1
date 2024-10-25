source("synthetic_data_generation.r")
source("nn.r")

library(tictoc)


# ---------- DATA PARAMETERS ----------

# Hardness 
hardness_values <- c(0.2, 1, 2)

# Noise
noise_levels <- c(0.2, 1, 2)

# Training sizes
training_sizes <- c(100, 1000, 10000)
validation_size <- 1000
test_size <- 1000

# ---------- RUN PARAMETERS ----------

# Number of hidden neurons
sizes <- c(1, 5, 10)

# Max number of iterations
maxits <- c(100, 200, 300) 

# Training functions
training_functions  <- c("train_nn")


# ---------- RESULT STRUCTURE ----------

results <- data.frame(
  hardness = numeric(),
  noise = numeric(),
  training_size = numeric(),
  size = numeric(),
  maxit = numeric(),
  training_function = character(),
  test_accuracy = numeric(),
  training_time = numeric(),
  stringsAsFactors = FALSE
)

# ---------- EVALUATION ----------

for (hardness in hardness_values) {
  for (noise in noise_levels) {
    for (training_size in training_sizes) {

      data <- generate_synthetic_data(training_size, validation_size, test_size, noise, hardness)
      
      train_data <- data$train
      val_data <- data$val
      test_data <- data$test

      for (size in sizes) {
        for (maxit in maxits) {
          for (training_function in training_functions) {
            
            # Retrieve the training function by name
            func <- get(training_function)
            
            # Measure training time using tic and toc
            tic() 
            model <- func(train_data, size, maxit)
            training_time <- toc(quiet = TRUE)$toc 
            
            # Test accuracy
            test_accuracy <- test_nn(model, test_data)
            
            # Store result
            results <- rbind(results, data.frame(
              hardness = hardness,
              noise = noise,
              training_size = training_size,
              size = size,
              maxit = maxit,
              training_function = training_function,
              test_accuracy = test_accuracy,
              training_time = training_time
            ))
          }
        }
      }
    }
  }
}

# Identify the fastest and most accurate runs
fastest_run <- results[which.min(results$training_time), ]
most_accurate_run <- results[which.max(results$test_accuracy), ]

# Print results
print("Fastest Run:")
print(fastest_run)

print("Most Accurate Run:")
print(most_accurate_run)


