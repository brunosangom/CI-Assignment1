suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(GGally))


generate_synthetic_data <- function(n_train, n_val, n_test, noise_level = 0.1, hardness = 0.5) {
    set.seed(123)

    mu_setosa <- c(5.01, 3.43, 1.46, 0.246)
    mu_versicolor <- c(5.94, 2.77, 4.26, 1.33)
    mu_virginica <- c(6.59, 2.97, 5.55, 2.03)

    mu_setosa <- mu_setosa - c(hardness, hardness, hardness, hardness)
    mu_virginica <- mu_virginica + c(hardness, hardness, hardness, hardness)

    setosa_cov <- matrix(c(
        0.12424898, 0.099216327, 0.016355102, 0.010330612,
        0.09921633, 0.143689796, 0.011697959, 0.009297959,
        0.01635510, 0.011697959, 0.030159184, 0.006069388,
        0.01033061, 0.009297959, 0.006069388, 0.011106122
    ), nrow = 4, byrow = TRUE)

    versicolor_cov <- matrix(c(
        0.26643265, 0.08518367, 0.18289796, 0.05577959,
        0.08518367, 0.09846939, 0.08265306, 0.04120408,
        0.18289796, 0.08265306, 0.22081633, 0.07310204,
        0.05577959, 0.04120408, 0.07310204, 0.03910612
    ), nrow = 4, byrow = TRUE)

    virginica_cov <- matrix(c(
        0.40434286, 0.09376327, 0.30328980, 0.04909388,
        0.09376327, 0.10400408, 0.07137959, 0.04762857,
        0.30328980, 0.07137959, 0.30458776, 0.04882449,
        0.04909388, 0.04762857, 0.04882449, 0.07543265
    ), nrow = 4, byrow = TRUE)

    X_setosa_train <- mvrnorm(n_train %/% 3, mu_setosa, setosa_cov)
    X_setosa_val <- mvrnorm(n_val %/% 3, mu_setosa, setosa_cov)
    X_setosa_test <- mvrnorm(n_test %/% 3, mu_setosa, setosa_cov)

    X_versicolor_train <- mvrnorm(n_train %/% 3, mu_versicolor, versicolor_cov)
    X_versicolor_val <- mvrnorm(n_val %/% 3, mu_versicolor, versicolor_cov)
    X_versicolor_test <- mvrnorm(n_test %/% 3, mu_versicolor, versicolor_cov)

    X_virginica_train <- mvrnorm(n_train %/% 3, mu_virginica, virginica_cov)
    X_virginica_val <- mvrnorm(n_val %/% 3, mu_virginica, virginica_cov)
    X_virginica_test <- mvrnorm(n_test %/% 3, mu_virginica, virginica_cov)

    X_train <- rbind(X_setosa_train, X_versicolor_train, X_virginica_train)
    y_train <- c(rep(0, n_train %/% 3), rep(1, n_train %/% 3), rep(2, n_train %/% 3))

    X_val <- rbind(X_setosa_val, X_versicolor_val, X_virginica_val)
    y_val <- c(rep(0, n_val %/% 3), rep(1, n_val %/% 3), rep(2, n_val %/% 3))

    X_test <- rbind(X_setosa_test, X_versicolor_test, X_virginica_test)
    y_test <- c(rep(0, n_test %/% 3), rep(1, n_test %/% 3), rep(2, n_test %/% 3))

    X_train <- X_train + rnorm(length(X_train), 0, noise_level)
    X_val <- X_val + rnorm(length(X_val), 0, noise_level)
    X_test <- X_test + rnorm(length(X_test), 0, noise_level)

    list(
        train = data.frame(X_train, y_train = as.factor(y_train)),
        val = data.frame(X_val, y_val = as.factor(y_val)),
        test = data.frame(X_test, y_test = as.factor(y_test))
    )
}

