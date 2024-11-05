# Load required libraries
library(readr)
library(ggplot2)
library(dplyr)
library(car)  
library(FSA)  
library(rstatix)
library(PMCMRplus)

# Function to write to file with console mirroring
write_and_print <- function(text, file_conn) {
  cat(text)
  cat(text, file = file_conn, append = TRUE)
}

# Start analysis and create output file
analyze_and_report <- function(alpha = 0.15) {  # Added alpha parameter with default value
  # Create output file
  output_file <- "statistical_analysis_report.txt"
  file_conn <- file(output_file, "w")
  
  write_and_print("Statistical Analysis Report for Training Functions\n", file_conn)
  write_and_print("==============================================\n\n", file_conn)
  write_and_print(sprintf("Using significance level (alpha) = %.3f\n\n", alpha), file_conn)
  
  # Read the data
  data <- read_csv("results.csv", show_col_types = FALSE)
  data$training_function <- factor(data$training_function)
  
  # Function to check assumptions and write explanation
  check_assumptions <- function(data, dv) {
    write_and_print(sprintf("\nChecking Statistical Assumptions for %s\n", dv), file_conn)
    write_and_print("----------------------------------------\n", file_conn)
    
    # Shapiro-Wilk test explanation
    write_and_print("\nPerforming Shapiro-Wilk test for normality:\n", file_conn)
    write_and_print("H0: Data is normally distributed\n", file_conn)
    write_and_print("H1: Data is not normally distributed\n\n", file_conn)
    
    normal_tests <- data %>%
      group_by(training_function) %>%
      summarise(
        shapiro_stat = shapiro.test(get(dv))$statistic,
        shapiro_p = shapiro.test(get(dv))$p.value
      )
    
    print(normal_tests)
    capture.output(print(normal_tests), file = file_conn, append = TRUE)
    
    # Interpret normality results
    write_and_print("\nNormality Test Interpretation:\n", file_conn)
    for(i in 1:nrow(normal_tests)) {
      result_text <- sprintf("- %s: %s (p = %.4f)\n",
                           normal_tests$training_function[i],
                           ifelse(normal_tests$shapiro_p[i] >= alpha, 
                                  "Normally distributed",
                                  "Not normally distributed"),
                           normal_tests$shapiro_p[i])
      write_and_print(result_text, file_conn)
    }
    
    # Levene's test explanation and execution
    write_and_print("\nPerforming Levene's test for homogeneity of variance:\n", file_conn)
    write_and_print("H0: Variances are equal across groups\n", file_conn)
    write_and_print("H1: Variances are not equal across groups\n\n", file_conn)
    
    levene_test <- leveneTest(as.formula(paste(dv, "~ training_function")), data = data)
    print(levene_test)
    capture.output(print(levene_test), file = file_conn, append = TRUE)
    
    # Interpret Levene's test
    levene_p <- levene_test$`Pr(>F)`[1]
    result_text <- sprintf("\nVariance Test Interpretation: %s (p = %.4f)\n",
                         ifelse(levene_p >= alpha,
                                "Equal variances across groups",
                                "Unequal variances across groups"),
                         levene_p)
    write_and_print(result_text, file_conn)
    
    # Return whether parametric tests are appropriate
    all_normal <- all(normal_tests$shapiro_p >= alpha)
    vars_homogeneous <- levene_p >= alpha
    
    write_and_print(sprintf("\nBased on these results, %s tests will be used.\n",
                          ifelse(all_normal && vars_homogeneous,
                                 "parametric",
                                 "non-parametric")), file_conn)
    
    return(all_normal && vars_homogeneous)
  }
  
  # Function to analyze a variable
  analyze_variable <- function(data, dv) {
    write_and_print(sprintf("\n\nAnalysis of %s\n", dv), file_conn)
    write_and_print("======================\n", file_conn)
    
    use_parametric <- check_assumptions(data, dv)
    
    if(use_parametric) {
      # ANOVA
      write_and_print("\nPerforming One-way ANOVA:\n", file_conn)
      write_and_print("H0: All group means are equal\n", file_conn)
      write_and_print("H1: At least one group mean is different\n\n", file_conn)
      
      aov_result <- aov(as.formula(paste(dv, "~ training_function")), data = data)
      print(summary(aov_result))
      capture.output(print(summary(aov_result)), file = file_conn, append = TRUE)
      
      # Interpret ANOVA
      aov_p <- summary(aov_result)[[1]]["training_function", "Pr(>F)"]
      result_text <- sprintf("\nANOVA Interpretation: %s (p = %.4f)\n",
                           ifelse(aov_p < alpha,
                                  "There are significant differences between groups",
                                  "No significant differences between groups"),
                           aov_p)
      write_and_print(result_text, file_conn)
      
      # Post-hoc tests
      if(aov_p < alpha) {
        write_and_print("\nPerforming Tukey's HSD test for pairwise comparisons:\n", file_conn)
        tukey_result <- TukeyHSD(aov_result)
        print(tukey_result)
        capture.output(print(tukey_result), file = file_conn, append = TRUE)
      }
      
    } else {
      # Kruskal-Wallis
      write_and_print("\nPerforming Kruskal-Wallis test:\n", file_conn)
      write_and_print("H0: All groups have the same distribution\n", file_conn)
      write_and_print("H1: At least one group has a different distribution\n\n", file_conn)
      
      kw_result <- kruskal.test(as.formula(paste(dv, "~ training_function")), data = data)
      print(kw_result)
      capture.output(print(kw_result), file = file_conn, append = TRUE)
      
      # Interpret Kruskal-Wallis
      result_text <- sprintf("\nKruskal-Wallis Test Interpretation: %s (p = %.4f)\n",
                           ifelse(kw_result$p.value < alpha,
                                  "There are significant differences between groups",
                                  "No significant differences between groups"),
                           kw_result$p.value)
      write_and_print(result_text, file_conn)
      
      # Post-hoc tests if significant
      if(kw_result$p.value < alpha) {
                write_and_print("\nPerforming Dunn's test for pairwise comparisons:\n", file_conn)
        write_and_print("H0: No difference between the pair of groups\n", file_conn)
        write_and_print("H1: Significant difference exists between the pair of groups\n\n", file_conn)
        
        dunn_result <- dunnTest(as.formula(paste(dv, "~ training_function")), 
                               data = data,
                               method = "bh")  # Using Benjamini-Hochberg adjustment
        print(dunn_result)
        capture.output(print(dunn_result), file = file_conn, append = TRUE)

        # Add interpretation of Dunn's test results
        write_and_print("\nDunn's Test Interpretation:\n", file_conn)
        write_and_print("-------------------------\n", file_conn)
        
        # Extract comparisons and p-values
        comparisons <- dunn_result$res
        
        # Sort by p-value for clearer presentation
        comparisons <- comparisons[order(comparisons$P.adj), ]
        
        for (i in 1:nrow(comparisons)) {
            comparison <- comparisons[i, ]
            sig_status <- if (comparison$P.adj < alpha) {
                "Significant difference"
            } else {
                "No significant difference"
            }

            # Properly parse the comparison string
            groups <- strsplit(as.character(comparison$Comparison), " - ")[[1]]
            group1 <- groups[1]
            group2 <- groups[2]

            # Calculate means with proper error handling
            mean1 <- tryCatch(
                {
                    mean(data[[dv]][data$training_function == group1], na.rm = TRUE)
                },
                error = function(e) NA
            )

            mean2 <- tryCatch(
                {
                    mean(data[[dv]][data$training_function == group2], na.rm = TRUE)
                },
                error = function(e) NA
            )

            # Determine which group performed better with NA handling
            better_group <- if (is.na(mean1) || is.na(mean2)) {
                "Unable to determine performance difference"
            } else if (mean1 > mean2) {
                sprintf(
                    "%s (%.3f) performed better than %s (%.3f)",
                    group1, mean1, group2, mean2
                )
            } else if (mean2 > mean1) {
                sprintf(
                    "%s (%.3f) performed better than %s (%.3f)",
                    group2, mean2, group1, mean1
                )
            } else {
                sprintf(
                    "%s and %s performed equally (%.3f)",
                    group1, group2, mean1
                )
            }

            result_text <- sprintf(
                "- %s vs %s: %s (p = %.4f)\n  %s\n",
                group1, group2,
                sig_status,
                comparison$P.adj,
                better_group
            )
            write_and_print(result_text, file_conn)
        }
        
        # Add overall summary of significant differences
        sig_pairs <- sum(comparisons$P.adj < alpha)
        total_pairs <- nrow(comparisons)
        write_and_print(sprintf("\nSummary: Found significant differences in %d out of %d comparisons.\n",
                              sig_pairs, total_pairs), file_conn)
      }
    }
    
    # Effect size
    write_and_print("\nCalculating effect size (Eta-squared):\n", file_conn)
    eff_size <- data %>% anova_test(as.formula(paste(dv, "~ training_function")))
    print(eff_size)
    capture.output(print(eff_size), file = file_conn, append = TRUE)
    
    # Interpret effect size
    ges <- eff_size$ges
    effect_size_interp <- case_when(
      ges < 0.01 ~ "negligible",
      ges < 0.06 ~ "small",
      ges < 0.14 ~ "medium",
      TRUE ~ "large"
    )
    write_and_print(sprintf("\nEffect size interpretation: %s effect (ges = %.3f)\n",
                          effect_size_interp, ges), file_conn)
  }
  
  # Perform analyses
  analyze_variable(data, "test_accuracy")
  analyze_variable(data, "training_time")
  
  # Summary statistics
  write_and_print("\n\nSummary Statistics\n", file_conn)
  write_and_print("=================\n", file_conn)
  
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
  capture.output(print(summary_stats), file = file_conn, append = TRUE)
  
  # Final conclusions
  write_and_print("\n\nFinal Conclusions\n", file_conn)
  write_and_print("================\n", file_conn)
  write_and_print("Test Accuracy:\n", file_conn)
  
  acc_means <- summary_stats$mean_accuracy
  names(acc_means) <- summary_stats$training_function
  best_acc <- names(which.max(acc_means))
  worst_acc <- names(which.min(acc_means))
  
  write_and_print(sprintf("- Best performing method: %s (%.3f)\n", 
                        best_acc, max(acc_means)), file_conn)
  write_and_print(sprintf("- Worst performing method: %s (%.3f)\n", 
                        worst_acc, min(acc_means)), file_conn)
  
  write_and_print("\nTraining Time:\n", file_conn)
  time_means <- summary_stats$mean_time
  names(time_means) <- summary_stats$training_function
  fastest <- names(which.min(time_means))
  slowest <- names(which.max(time_means))
  
  write_and_print(sprintf("- Fastest method: %s (%.2f)\n", 
                        fastest, min(time_means)), file_conn)
  write_and_print(sprintf("- Slowest method: %s (%.2f)\n", 
                        slowest, max(time_means)), file_conn)
  
  # Close the connection
  close(file_conn)
  cat("\nReport has been saved to", output_file, "\n")
}

# Run the analysis with alpha = 0.15
analyze_and_report(0.15)