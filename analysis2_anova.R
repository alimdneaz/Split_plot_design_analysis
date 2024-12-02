library(agricolae)


df <- read.csv("./split_plot.csv", header = T)
df$Rep <- as.factor(df$Rep)
df$Tillage <- as.factor(df$Tillage)
df$Organic_Manure <- as.factor(df$Organic_Manure)

split_plot <- with(df,design.split(
  trt1 = df$Tillage,
  trt2 = df$Tillage,
  r = df$Rep
))

split_plot$ANOVA

library(lmerTest)

# Fit the model
model <- lmer(Yield ~ Tillage * Organic_Manure + (1 | Rep) + (1 | Rep:Tillage), data = df)

# Perform ANOVA with type-II or type-III sum of squares
anova_table <- anova(model, type = 3)
print(anova_table)

model <- lm(df$Yield ~ df$Tillage + Error(df$Tillage)+ df$Organic_Manure + df$Tillage:df$Organic_Manure)


model <- aov(Yield ~ Tillage * Organic_Manure + Error(Rep/Tillage), data = df)
summary(model)