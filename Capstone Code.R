# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(kableExtra)
library(reshape2)
library(GGally)

# 1. Load and filter data
df <- read_excel("GA_Hospitals_Final.xlsx", sheet = "GA_Hospitals")

# 2. Transform variables
df <- df %>%
  mutate(
    Log_Fill_Rate = log(Fill_Rate),
    Log_Income = log(Median_Income),
    Log_Population = log(Population),
    Profit_Type = as.factor(Profit_Type),
    Hospital_System_Affiliation = as.factor(Hospital_System_Affiliation),
    Academic_Medical_Center = as.factor(Academic_Medical_Center)
  )

# 3. Prepare modeling dataset
model_data <- df %>%
  select(Log_Fill_Rate, Number_of_Beds, Medicare_Pct, Log_Income, Unemployment_Rate, 
         Bachelor_or_Higher, Log_Population, Population_Over_65, Uninsured_Rate,
         Profit_Type, Hospital_System_Affiliation, Academic_Medical_Center)

# 4. Fit multiple linear regression model
model <- lm(Log_Fill_Rate ~ ., data = model_data)
summary_model <- summary(model)
adjusted_r2 <- summary_model$adj.r.squared
print(adjusted_r2)
print(summary(model))

# 5. Create a clean results table with CIs
results_table <- tidy(model, conf.int = TRUE) %>%
  mutate(
    term = gsub("_", " ", term),
    Estimate = round(estimate, 3),
    Std_Error = round(std.error, 3),
    CI_Lower = round(conf.low, 3),
    CI_Upper = round(conf.high, 3),
    P_Value = round(p.value, 3)
  ) %>%
  select(term, Estimate, Std_Error, CI_Lower, CI_Upper, P_Value)

# Print the regression table nicely
results_table %>%
  kable(caption = "Linear Regression Results with 95\\% Confidence Intervals for Natural Log(Fill Rate)",
        col.names = c("Predictor", "Estimate", "Std. Error", "CI Lower", "CI Upper", "p-value")) %>%
  kable_styling(full_width = FALSE, position = "center")

# 6. Diagnostic plots
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# 7. Coefficient plot
coef_plot <- tidy(model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = gsub("_", " ", term))

ggplot(coef_plot, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Coefficient Plot with 95% Confidence Intervals", x = "Estimate", y = "Predictor") +
  theme_minimal()

# 8. Histogram of natural log(Fill Rate)
ggplot(df, aes(Log_Fill_Rate)) +
  geom_histogram(binwidth = 0.04, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Log(Fill Rate)", x = "Log(Fill Rate)", y = "Number of Hospitals") +
  theme_minimal()

# 9. Boxplot of raw Fill Rate by Profit Type
ggplot(df, aes(x = Profit_Type, y = Fill_Rate, fill = Profit_Type)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("FOR-PROFIT" = "darkred", "NON-PROFIT" = "steelblue")) +
  labs(title = "Fill Rate by Ownership Type", x = "Ownership Type", y = "Fill Rate") +
  theme_minimal()

# 10. Scatterplot: log(Fill Rate) vs log(Income)
ggplot(df, aes(x = Log_Income, y = Log_Fill_Rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Log(Fill Rate) vs Log(Median Income)", x = "Log(Median Income)", y = "Natural Log(Fill Rate)") +
  theme_minimal()

# 11. Correlation heatmap of numeric variables
numeric_vars <- c("Number_of_Beds", "Medicare_Pct", "Median_Income", "Unemployment_Rate", 
                  "Bachelor_or_Higher", "Population", "Population_Over_65", "Uninsured_Rate")

cor_data <- df %>% select(all_of(numeric_vars)) %>% na.omit()
cor_matrix <- cor(cor_data)
cor_melted <- reshape2::melt(cor_matrix)

cor_melted$Var1 <- gsub("_", " ", cor_melted$Var1)
cor_melted$Var2 <- gsub("_", " ", cor_melted$Var2)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "steelblue", high = "darkred", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Numeric Variables", x = "", y = "")
