library(nnet)
source("synthetic_data_generation.r")


n_train <- 100  
n_val <- 1000   
n_test <- 1000
noise_level <- 0.3
hardness <- 0.7 

data <- generate_synthetic_data(n_train, n_val, n_test, noise_level, hardness)
train_data <- data$train
val_data <- data$val
test_data <- data$test


# Size - Number of hidden neurons
# Maxit - Maximum number of iterations or epochs for the training process
# Decay - Regularization parameter, which penalizes large weights
train_nn <- function(train_data, size = 1, maxit = 200, decay = 0.0001) {
  train_data$y_train <- as.factor(train_data$y_train)
  model <- nnet(y_train ~ ., 
                data = train_data, 
                size = size, 
                maxit = maxit, 
                decay = decay, 
                linout = FALSE)
  return(model)
}

hyperparameter_search <- function(train_data, val_data, sizes, maxits) {
  best_model <- NULL
  best_accuracy <- 0
  best_params <- NULL
  
  for (size in sizes) {
    for (maxit in maxits) {
        model <- train_nn(train_data, size, maxit)
        
        y_pred <- predict(model, newdata = val_data[, -ncol(val_data)], type = "class")
        accuracy <- accuracy <- mean(y_pred == val_data[, ncol(val_data)])

        
        if (accuracy > best_accuracy) {
            best_accuracy <- accuracy
            best_model <- model
            best_params <- list(size = size, maxit = maxit)
        }
    }

}
  
  return(list(best_model = best_model, best_accuracy = best_accuracy, best_params = best_params))
}


test_nn <- function(best_model, test_data, size = 1, maxit = 200, decay = 0.0001, random_state=42){

    y_pred = predict(best_model, newdata = test_data[, -ncol(test_data)], type = "class")
    
    accuracy = accuracy <- mean(y_pred == test_data[, ncol(test_data)])
    
    cat(sprintf("Test Accuracy: %.4f\n", accuracy))
    
    return(accuracy)
}


# Hyperparameter search
sizes <- c(1, 5, 10)       
maxits <- c(100, 200, 300) 

res_hyperparameter_search <- hyperparameter_search(train_data, val_data, sizes, maxits)

best_model <- res_hyperparameter_search$best_model      
best_accuracy <- res_hyperparameter_search$best_accuracy
best_params <- res_hyperparameter_search$best_params   

# Test
test_accuracy <- test_nn(best_model, test_data, best_params$size, best_params$maxit)




