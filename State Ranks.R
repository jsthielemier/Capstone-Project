library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(caret)
library(readxl)
library(reshape2)

df <- Master_Table

# Scatter Plot: Relationship between GME Percentage Stayed in State and Physicians Rank
ggplot(df, aes(x = `GME Percentage Stayed in State`, y = `Physicians Rank`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relationship between GME Percentage Stayed in State and Physicians Rank",
       x = "GME Percentage Stayed in State", y = "Physicians Rank")

# Scatter Plot: Relationship between UME Per Capita and Physicians Rank
ggplot(df, aes(x = `UME Per Capita (100,000)`, y = `Physicians Rank`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Relationship between UME Per Capita and Physicians Rank",
       x = "UME Per Capita (100,000)", y = "Physicians Rank")

# Ensure Physicians Rank is numeric
df$`Physicians Rank` <- as.numeric(df$`Physicians Rank`)

# Histogram: Distribution of Physicians Rank
ggplot(df, aes(x = `Physicians Rank`)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Physicians Rank", x = "Physicians Rank", y = "Frequency")


# Correlation Heatmap: Among all numeric variables
# Select numeric columns only and calculate correlation
numeric_df <- df %>% select(where(is.numeric))
cor_matrix <- cor(numeric_df, use = "complete.obs")

# Melt correlation matrix for ggplot
melted_cor <- melt(cor_matrix)
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Numeric Variables") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))








df <- df %>%
  mutate(across(everything(), ~ na_if(., "---"))) %>%
  mutate(across(everything(), ~ na_if(., "N.R.")))

# Convert columns to numeric where applicable
numeric_cols <- c("Total Population", "Total Students Enrolled in UME", "UME Per Capita (100,000)",
                  "UME Rank", "Total Residents in GME", "GME Per Capita (100,000)", "GME Rank",
                  "UME Percentage Stayed in State", "UME Grad Rank", "GME Percentage Stayed in State",
                  "GME Completed Rank", "GME and UME Percentage Stayed in State", "GME and UME Rank",
                  "Physicians Per Capita (100,000)", "Physicians Rank")
df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)

# Drop rows with NA in the target variable
df <- df %>% drop_na(`Physicians Rank`)

# Define the predictor variables and target variable
predictors <- df %>%
  select(`Total Population`, `Total Students Enrolled in UME`, `UME Per Capita (100,000)`,
         `Total Residents in GME`, `GME Per Capita (100,000)`, `UME Percentage Stayed in State`,
         `GME Percentage Stayed in State`, `GME and UME Percentage Stayed in State`, 
         `Physicians Per Capita (100,000)`)

target <- df$`Physicians Rank`

# Impute missing values in predictors using median imputation
preprocess <- preProcess(predictors, method = "medianImpute")
predictors_imputed <- predict(preprocess, predictors)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(target, p = 0.7, list = FALSE)
train_data <- predictors_imputed[trainIndex, ]
test_data <- predictors_imputed[-trainIndex, ]
train_target <- target[trainIndex]
test_target <- target[-trainIndex]

# Train the linear model
model <- train(train_data, train_target, method = "lm")

# Evaluate the model
predictions <- predict(model, test_data)
mse <- mean((test_target - predictions)^2)
r2 <- cor(test_target, predictions)^2

# Display model summary
summary(model$finalModel)
cat("MSE:", mse, "\n")
cat("R-squared:", r2, "\n")









dat$`UME Rank` <- as.numeric(as.character(dat$`UME Rank`))
dat$`GME Rank` <- as.numeric(as.character(dat$`GME Rank`))
dat$`Physicians Rank` <- as.numeric(as.character(dat$`Physicians Rank`))
dat <- dat[complete.cases(dat$`UME Rank`, dat$`GME Rank`, dat$`Physicians Rank`), ]


cor(dat$`UME Rank`, dat$`Physicians Rank`, method = "spearman")
plot(dat$`UME Rank`, dat$`Physicians Rank`,
     main = "Scatterplot of UME Rank vs Physicians Rank",
     xlab = "UME Rank",
     ylab = "Physicians Rank",
     pch = 19, col = "blue")
cor.test(dat$`UME Rank`, dat$`Physicians Rank`, method = "spearman")


cor(dat$`GME Rank`, dat$`Physicians Rank`, method = "spearman")
plot(dat$`GME Rank`, dat$`Physicians Rank`,
     main = "Scatterplot of GME Rank vs Physicians Rank",
     xlab = "GME Rank",
     ylab = "Physicians Rank",
     pch = 19, col = "blue")
cor.test(dat$`GME Rank`, dat$`Physicians Rank`, method = "spearman")

