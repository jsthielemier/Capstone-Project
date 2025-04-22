library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glmnet)
library(caret)
library(broom)
library(knitr)
library(kableExtra)
library(ggrepel)
library(scales)

df <- read_excel("C:/Users/Jacob Thielemier/OneDrive - Hull Property Group/Desktop/Capstone-Project/GA_Hospitals_Final.xlsx")

# Define features
numeric_vars <- c("Number_of_Beds", "Medicare_Days", "Medicaid_Days",
                  "Median_Income", "Unemployment_Rate", "Poverty_Rate",
                  "Bachelor_or_Higher", "Population", "Population_Over_65", "Uninsured_Rate")

categorical_vars <- c("Urban_Rural_Status", "Ownership_Type", 
                      "Hospital_System_Affiliation", "Academic_Medical_Center")

# Recode categorical vars as factors
df <- df %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Build modeling frame
model_data <- df %>%
  select(Filled_Slots, all_of(numeric_vars), all_of(categorical_vars))

# Prepare matrices
x <- model.matrix(Filled_Slots ~ ., data = model_data)[, -1]
y <- model_data$Filled_Slots

# Lasso with cross-validation
set.seed(42)
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Best lambda
lasso_model$lambda.min

# Coefficients
coef(lasso_model, s = "lambda.min")

# Fit OLS model
ols_model <- lm(Filled_Slots ~ ., data = model_data)

# Extract tidy summary
ols_results <- broom::tidy(ols_model)

# Display clean table
ols_results %>%
  mutate(term = str_replace(term, "`", "")) %>%
  kable(digits = 4, caption = "OLS Regression Coefficients and P-Values for Filled_Slots") %>%
  kable_styling(full_width = FALSE)

ggplot(model_data, aes(x = Filled_Slots, y = fitted(ols_model))) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(
    title = "Actual vs Predicted Filled Residency Slots",
    x = "Actual Filled Slots",
    y = "Predicted Filled Slots"
  ) +
  theme_minimal()

ols_results %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Predictor Effects on Filled Residency Slots",
       x = "Predictor Variable", y = "Coefficient Estimate") +
  theme_minimal()
