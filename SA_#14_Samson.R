# SA - #14

library(ggplot2)
library(car)            # For Levene's test
library(dplyr)          # For data manipulation
library(stats)          # For ANOVA and Shapiro-Wilk test
library(tidyverse)      # For data wrangling
library(DescTools)      # For Post-hoc Tukey Test

file_path <- "C:\\Users\\User\\OneDrive\\Personal docs\\FRESHMAN\\4th yr - 1st Sem\\Applied Multivariate Data Analysis\\SA1_Samson\\RatExploration_csvFile.csv"
rat_data <- read.csv(file_path)

# Clean the data by removing the unnecessary "X" column
rat_data_cleaned <- rat_data %>% select(-X)

# Boxplot to visualize exploration time by stimuli type
ggplot(rat_data_cleaned, aes(x = Stimuli, y = Time, fill = Stimuli)) + 
  geom_boxplot() +
  ggtitle("Boxplot of Exploration Time by Stimuli Type") +
  xlab("Stimuli Type") + 
  ylab("Exploration Time (seconds)")

# Shapiro-Wilk test for normality (for each group)
shapes_time <- subset(rat_data_cleaned, Stimuli == "Shape")$Time
patterns_time <- subset(rat_data_cleaned, Stimuli == "Pattern")$Time
pictures_time <- subset(rat_data_cleaned, Stimuli == "Picture")$Time

shapiro_shapes <- shapiro.test(shapes_time)
shapiro_patterns <- shapiro.test(patterns_time)
shapiro_pictures <- shapiro.test(pictures_time)

# Print Shapiro-Wilk results
cat("Shapiro-Wilk Test Results:\n")
print(shapiro_shapes)
print(shapiro_patterns)
print(shapiro_pictures)

# Levene's test for homogeneity of variances
levene_test <- leveneTest(Time ~ Stimuli, data = rat_data_cleaned)
cat("\nLevene's Test for Homogeneity of Variances:\n")
print(levene_test)

# One-way ANOVA
anova_results <- aov(Time ~ Stimuli, data = rat_data_cleaned)
cat("\nOne-Way ANOVA Results:\n")
summary(anova_results)

# Post-hoc Tukey HSD test if ANOVA is significant
if (summary(anova_results)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_test <- TukeyHSD(anova_results)
  cat("\nTukey HSD Post-hoc Results:\n")
  print(tukey_test)
}
