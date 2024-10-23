library(dplyr)

data <- iris

# Assuming 'data' is the Iris dataset
print(
    data %>%
    group_by(Species) %>%
    summarise(across(where(is.numeric), mean))    
)
