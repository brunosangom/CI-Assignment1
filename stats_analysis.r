# Load required libraries
library(readr)
library(ggplot2)
library(dplyr)
library(car)  
library(FSA)  
library(rstatix)
library(PMCMRplus)

# Read the data
data <- read_csv("results.csv", show_col_types = FALSE)

# Ensure training_function is a factor with proper levels
data$training_function <- factor(data$training_function)

# Function to check for infinite or NA values
check_data_validity <- function(data, dv) {
  invalid_counts <- data %>%
    summarise(
      na_count = sum(is.na(get(dv))),
      inf_count = sum(is.infinite(get(dv)))
    )
  
  if(invalid_counts$na_count > 0 || invalid_counts$inf_count > 0) {
    cat("\nWarning: Found", invalid_counts$na_count, "NA values and", 
        invalid_counts$inf_count, "infinite values in", dv, "\n")
    return(FALSE)
  }
  return(TRUE)
}

# Function to perform normality and variance tests
check_assumptions <- function(data, dv) {
  cat("\nChecking assumptions for", dv, "\n")
  cat("===============================\n")
  
  # Shapiro-Wilk test for each group
  cat("Normality Tests (Shapiro-Wilk):\n")
  normal_tests <- data %>%
    group_by(training_function) %>%
    summarise(
      shapiro_stat = shapiro.test(get(dv))$statistic,
      shapiro_p = shapiro.test(get(dv))$p.value
    )
  print(normal_tests)
  
  # Levene's test for homogeneity of variance
  cat("\nHomogeneity of Variance (Levene's Test):\n")
  formula <- as.formula(paste(dv, "~ training_function"))
  levene_test <- leveneTest(formula, data = data)
  print(levene_test)
  
  # Return whether parametric tests are appropriate
  all_normal <- all(normal_tests$shapiro_p >= 0.05)
  vars_homogeneous <- levene_test$`Pr(>F)`[1] >= 0.05
  
  return(all_normal && vars_homogeneous)
}

# Function to create and save plot
create_plot <- function(data, dv) {
  # Create enhanced boxplot
  p <- ggplot(data, aes(x = training_function, y = .data[[dv]])) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "none"
    ) +
    labs(
      title = paste("Distribution of", dv, "by Training Function"),
      x = "Training Function",
      y = gsub("_", " ", toupper(dv))  # Makes the y-axis label more readable
    )
  
  # Save plot with high resolution
  filename <- paste0(dv, "_distribution.png")
  ggsave(
    filename = filename,
    plot = p,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "white"
  )
  
  # Also display the plot in R
  print(p)
  
  cat("\nPlot saved as:", filename, "\n")
}

# Function to perform statistical tests and create visualizations
analyze_variable <- function(data, dv) {
  cat("\nAnalysis for", dv, "\n")
  cat("===============================\n")
  
  # Check data validity
  if(!check_data_validity(data, dv)) {
    cat("Skipping analysis due to invalid data\n")
    return()
  }
  
  # Create and save plot
  create_plot(data, dv)
  
  # Check if we should use parametric tests
  use_parametric <- check_assumptions(data, dv)
  
  if(use_parametric) {
    # One-way ANOVA
    cat("\nOne-way ANOVA:\n")
    formula <- as.formula(paste(dv, "~ training_function"))
    aov_result <- aov(formula, data = data)
    print(summary(aov_result))
    
    # Dunnett's test (comparing against BP as control)
    cat("\nDunnett's Test (comparing against BP as control):\n")
    tryCatch({
      # Ensure data is properly formatted for Dunnett's test
      dunnett_data <- data %>%
        drop_na(!!sym(dv)) %>%
        filter(is.finite(!!sym(dv)))
      
      dunnett <- glht(aov_result, 
                      linfct = mcp(training_function = "Dunnett"),
                      alternative = "two.sided")
      print(summary(dunnett))
    }, error = function(e) {
      cat("Error in Dunnett's test:", conditionMessage(e), "\n")
      cat("Falling back to Tukey's HSD test:\n")
      print(TukeyHSD(aov_result))
    })
    
  } else {
    # Kruskal-Wallis test
    cat("\nKruskal-Wallis Test:\n")
    kw_result <- kruskal.test(as.formula(paste(dv, "~ training_function")), data = data)
    print(kw_result)
    
    # Dunn's test
    cat("\nDunn's Test with Benjamini-Hochberg adjustment:\n")
    dunn_result <- dunnTest(as.formula(paste(dv, "~ training_function")), 
                           data = data,
                           method = "bh")
    print(dunn_result)
  }
  
  # Calculate effect sizes
  cat("\nEffect Sizes (Eta-squared):\n")
  eff_size <- data %>% anova_test(as.formula(paste(dv, "~ training_function")))
  print(eff_size)
}

# Perform analysis for both variables
analyze_variable(data, "test_accuracy")
analyze_variable(data, "training_time")

# Create summary statistics
cat("\nSummary Statistics:\n")
summary_stats <- data %>%
  group_by(training_function) %>%
  summarise(
    n = n(),
    mean_accuracy = mean(test_accuracy),
    sd_accuracy = sd(test_accuracy),
    mean_time = mean(training_time),
    sd_time = sd(training_time)
  )
print(summary_stats)