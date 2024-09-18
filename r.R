# Load necessary libraries
library(caret)       # for train/test splitting
library(Metrics)     # for RMSE, MAE
library(ggplot2)     # for plotting
library(MASS)        # for lm.ridge
library(glmnet)      # for lasso and elasticnet
library(e1071)       # for skewness
library(ggplot2)

# Load your dataset
# Replace 'your_data.csv' with your actual data file
setwd("D:/rpdbs T3")
data <- read.csv("data_YesBank_StockPrices.csv")
# Load necessary libraries


# Inspect the first few rows of the data
head(data)

# Ensure the target column `Close` is numeric
data$Close <- as.numeric(data$Close)

# Remove rows with NA values in any column
data <- na.omit(data)

# Create data partition (80% training, 20% testing)
set.seed(123)  # Set seed for reproducibility
trainIndex <- createDataPartition(data$Close, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Fit the linear regression model
model <- lm(Close ~ Open + High + Low, data = trainData)

# Predict on the test set
predictions <- predict(model, newdata = testData)

# Evaluate model performance
mse <- mean((testData$Close - predictions)^2)
rmse <- sqrt(mse)
r2 <- 1 - (sum((testData$Close - predictions)^2) / sum((testData$Close - mean(testData$Close))^2))
mae <- mean(abs(testData$Close - predictions))

# Print evaluation metrics
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print(paste("R2:", r2))
print(paste("MAE:", mae))

# Create a data frame for plotting
plot_data <- data.frame(
  Actual = testData$Close,
  Predicted = predictions
)

# Plot the predictions vs actual values
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Prices",
       x = "Actual Prices",
       y = "Predicted Prices") +
  theme_minimal()
