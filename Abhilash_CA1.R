# Create a vector for students' IDs
student_ids <- c(1:17)

# Create vectors for students' scores with and without visual aids
scores_no_aids <- c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61)
scores_with_aids <- c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)

# Combine into a data frame
performance_scores <- data.frame(
  Students = student_ids,
  No_Visual_Aids = scores_no_aids,
  Visual_Aids = scores_with_aids
)

# Display the data frame
print(performance_scores)

# Load psych package or install if it's not already installed
if (!require("psych", quietly = TRUE)) {
  install.packages("psych", dependencies = TRUE)
  library(psych)
}

# Generate descriptive statistics for scores
stats_description <- describe(performance_scores[, c("No_Visual_Aids", "Visual_Aids")])
print(stats_description)

# Create boxplots for comparison
boxplot(performance_scores$Visual_Aids, performance_scores$No_Visual_Aids,
        names = c("With Visual Aids", "Without Visual Aids"),
        main = "Comparison of Scores with and without Visual Aids",
        ylab = "Scores",
        col = c("green", "orange"))

# Perform Kolmogorov-Smirnov normality tests against a normal distribution
ks_test_with <- ks.test(performance_scores$Visual_Aids, "pnorm", mean(performance_scores$Visual_Aids), sd(performance_scores$Visual_Aids))
ks_test_without <- ks.test(performance_scores$No_Visual_Aids, "pnorm", mean(performance_scores$No_Visual_Aids), sd(performance_scores$No_Visual_Aids))
print(ks_test_with)
print(ks_test_without)

# Decide which statistical test to use based on normality results
if (ks_test_with$p.value > 0.05 && ks_test_without$p.value > 0.05) {
  stats_test <- t.test(performance_scores$Visual_Aids, performance_scores$No_Visual_Aids, paired = TRUE)
} else {
  stats_test <- wilcox.test(performance_scores$Visual_Aids, performance_scores$No_Visual_Aids, paired = TRUE)
}

# Output the test results
if (exists("stats_test")) {
  print(stats_test)
} else {
  cat("Statistical test results are not available.\n")
}