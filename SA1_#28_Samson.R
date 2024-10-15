# Load necessary libraries
library(ggplot2)
library(car)           # For Levene's test
library(dplyr)         # For data manipulation
library(stats)         # For ANOVA and Shapiro-Wilk test
library(DescTools)     # For post-hoc Tukey test

# Create the dataset
data <- data.frame(
  Participant = 1:20,
  Diet.Type = factor(rep(c("Low-Carb", "Low-Fat"), each = 10)),
  Exercise.Intensity = factor(rep(c("Low Intensity", "High Intensity"), times = 10)),
  Weight.Loss = c(2.5, 2.8, 2.9, 3.0, 2.7, 5.0, 4.8, 5.2, 4.7, 5.1,
                  1.5, 1.8, 1.4, 1.6, 1.7, 3.5, 3.7, 3.8, 3.6, 3.9)
)

# Step 1: Check Assumptions

# Shapiro-Wilk Test for Normality (for each group)
lowcarb_lowintensity <- subset(data, Diet.Type == "Low-Carb" & Exercise.Intensity == "Low Intensity")$Weight.Loss
lowcarb_highintensity <- subset(data, Diet.Type == "Low-Carb" & Exercise.Intensity == "High Intensity")$Weight.Loss
lowfat_lowintensity <- subset(data, Diet.Type == "Low-Fat" & Exercise.Intensity == "Low Intensity")$Weight.Loss
lowfat_highintensity <- subset(data, Diet.Type == "Low-Fat" & Exercise.Intensity == "High Intensity")$Weight.Loss

shapiro_test_results <- list(
  "Low-Carb, Low Intensity" = shapiro.test(lowcarb_lowintensity),
  "Low-Carb, High Intensity" = shapiro.test(lowcarb_highintensity),
  "Low-Fat, Low Intensity" = shapiro.test(lowfat_lowintensity),
  "Low-Fat, High Intensity" = shapiro.test(lowfat_highintensity)
)

# Print Shapiro-Wilk test results
shapiro_test_results

# Levene's Test for Homogeneity of Variance
levene_test <- leveneTest(Weight.Loss ~ Diet.Type * Exercise.Intensity, data = data)
print(levene_test)

# Step 2: Perform the 2x2 ANOVA
anova_model <- aov(Weight.Loss ~ Diet.Type * Exercise.Intensity, data = data)
summary(anova_model)

# Step 3: Post-hoc test if there's a significant interaction effect
if (summary(anova_model)[[1]]$'Pr(>F)'[3] < 0.05) {
  tukey_test <- TukeyHSD(anova_model, which = "Diet.Type:Exercise.Intensity")
  print(tukey_test)
}

# Step 4: Visualizing the interaction (optional)
interaction_plot <- ggplot(data, aes(x = Diet.Type, y = Weight.Loss, color = Exercise.Intensity)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = Exercise.Intensity)) +
  ggtitle("Interaction Plot: Diet Type and Exercise Intensity on Weight Loss") +
  xlab("Diet Type") + 
  ylab("Mean Weight Loss (kg)") +
  theme_minimal()

# Print the interaction plot
print(interaction_plot)
