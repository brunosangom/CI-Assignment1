library(dplyr)

data <- iris

# Assuming 'data' is the Iris dataset
print(
    data %>%
    group_by(Species) %>%
    summarise(across(where(is.numeric), mean))    
)

cov_matrices <- data %>%
    group_by(Species) %>%
    summarise(cov_matrix = list(cov(select(cur_data(), where(is.numeric))))) %>%
    pull(cov_matrix)

# `cov_matrices` will contain the covariance matrix for each Species as a list of matrices
names(cov_matrices) <- unique(data$Species)
print(cov_matrices)