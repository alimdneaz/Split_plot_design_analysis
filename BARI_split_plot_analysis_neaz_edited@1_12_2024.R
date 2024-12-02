###################################
#                                 #  
#   Split Plot Design BARI        #
#   Md. Neaz Ali                  #
#   M.SC - 2021-22                #
#   Roll: 212248                  #
#   Reg: 1591                     #
#                                 #
###################################

# Data Import
df <- read.csv("./split_plot.csv", header = T)
head(df)
str(df)
dim(df)
# Data Cleaning
df$Rep <- as.factor(df$Rep)
df$Tillage <- as.factor(df$Yield)

table(df$Rep)
table(df$Tillage)

# Importing Necessary Libraries

install.packages("lme4")
install.packages("lmerTest")  # For p-values
install.packages("emmeans")  # For pairwise comparisons
library(lme4)
library(lmerTest)
library(emmeans)


# Fit the Mixed-Effect Model
model <- lmer(df$Yield ~ df$Rep + df$Tillage + (1|df$Rep))
summary(model)

anova(model)

# Post_Hoc Tests
emmeans(model, pairwise ~ Rep)
emmeans(model, pairwise ~ Tillage)

# Visualize Results

library(ggplot2)
ggplot(df, aes(x = Tillage, y = Yield, color = Rep))+
  stat_summary(fun = mean, geom = "point", size = 3)+
  stat_summary(fun = mean, geom = "line", aes(group = Rep))+
  theme_minimal()+
  labs(title = "Interaction Plot", x = "Variety", y = "Yield")



# Diagnostic
plot(model)
qqnorm(residuals(model))
qqline(residuals(model))





# Boxplots of the data --------------------------------------------------------
boxplot(df$Yield)
boxplot(df$Yield ~ df$Rep)
boxplot(df$Yield ~ df$Tillage)

# Residual Plot
# Extract residuals and fitted values
residuals_df <- data.frame(
  Fitted = fitted(model),
  Residuals = resid(model)
)

ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Residual points
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # Reference line
  labs(
    title = "Residual Plot",
    subtitle = "Checking for homogeneity of variance",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(color = "black")
  )


# Q-Q Plot
# Create Q-Q Plot
qq_data <- data.frame(
  Theoretical = qqnorm(residuals(model), plot.it = FALSE)$x,
  Sample = qqnorm(residuals(model), plot.it = FALSE)$y
)

ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.7) +  # Q-Q points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +  # Reference line
  labs(
    title = "Q-Q Plot",
    subtitle = "Checking for normality of residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(color = "black")
  )

# Aesthetic Boxplot
ggplot(df, aes(x = Tillage, y = Yield, fill = Tillage)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 3) +  # Add boxplot with styled outliers
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "black", stroke = 1.5) +  # Add mean as points
  scale_fill_manual(values = c("skyblue", "lightgreen")) +  # Customize fill colors
  labs(
    title = "Effect of Tillage on Crop Yield",
    subtitle = "Comparing Conventional vs. Conservation Tillage",
    x = "Tillage Type",
    y = "Yield (Units)",
    fill = "Tillage"
  ) +
  theme_minimal(base_size = 14) +  # Use minimal theme with larger base font size
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Center and bold the title
    plot.subtitle = element_text(size = 12, hjust = 0.5),  # Center the subtitle
    axis.text = element_text(color = "black"),  # Black axis labels
    legend.position = "top",  # Place legend at the top
    legend.title = element_text(face = "bold")  # Bold legend title
  ) +
  coord_flip()  # Flip coordinates for horizontal boxplots (optional)