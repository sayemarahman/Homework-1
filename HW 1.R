
# load libraries
library(class)
library(ggplot2)
library(crayon)

# load data
training_data <- read.csv("C:/Users/sayem/Downloads/1-training_data.csv")
test_data <- read.csv("C:/Users/sayem/Downloads/1-test_data.csv")

# get features and labels
train_features <- training_data[, -ncol(training_data)]
train_labels <- training_data[, ncol(training_data)]
test_features <- test_data[, -ncol(training_data)]
test_labels <- test_data[, ncol(training_data)]

# (a) Fit KNN with K = 1, 6, 11, ..., 200
kvals <- seq(1, 200, by = 5)
train_errors <- test_errors <- numeric(length(kvals))

for (i in seq_along(kvals)) {
  k <- kvals[i]
  
  # Train KNN model
  knn_model <- knn(train = train_features, test = train_features, cl = train_labels, k = k)
  
  # Predict on training data
  train_pred <- as.factor(knn_model)
  
  # Predict on test data
  test_pred <- knn(train = train_features, test = test_features, cl = train_labels, k = k)
  
  # Calculate error rates
  train_errors[i] <- mean(train_pred != train_labels)
  test_errors[i] <- mean(test_pred != test_labels)
}
# (b) Plot Training and Test Error Rates against K
error_plot <- data.frame(K = kvals, Train_Error = train_errors, Test_Error = test_errors)

ggplot(error_plot, aes(x = K)) +
  geom_line(aes(y = Train_Error, color = "Training Error")) +
  geom_line(aes(y = Test_Error, color = "Test Error")) +
  labs(title = "Training and Test Error Rates vs. K",
       x = "K (Number of Neighbors)",
       y = "Error Rate") + 
  theme_minimal()

# (c) Optimal Value of K and Associated Error Rates
optimal_k <- kvals[which.min(test_errors)]
optimal_train_error <- train_errors[which.min(test_errors)]
optimal_test_error <- min(test_errors)

cat("Optimal Value of K:", optimal_k, "\n")
cat("Training Error with Optimal K:", optimal_train_error, "\n")
cat("Test Error with Optimal K:", optimal_test_error, "\n")

# (d) Plot Training Data with Decision Boundary for Optimal K
# Train KNN model with optimal k
optimal_k <- 131
knn_model <- knn(train = train_features, test = train_features, cl = train_labels, k = optimal_k)

# Define a grid of points
x1_grid <- seq(min(train_features[, 1]) - 1, max(train_features[, 1]) + 1, length.out = 100)
x2_grid <- seq(min(train_features[, 2]) - 1, max(train_features[, 2]) + 1, length.out = 100)
grid_points <- expand.grid(x1 = x1_grid, x2 = x2_grid)

# Predict class labels for grid points
grid_pred <- knn(train = train_features, test = grid_points, cl = train_labels, k = optimal_k)

# Plot training data with decision boundary
decision_boundary_plot <- ggplot() +
  geom_point(data = training_data, aes(x = x1, y = x2, color = factor(Class)), size = 3) +
  geom_contour(data = grid_points, aes(x = x1, y = x2, z = as.factor(grid_pred)), color = "black") +
  labs(title = "Training Data with Decision Boundary (K = 131)",
       x = "x.1",
       y = "x.2",
       color = "Class") +
  theme_minimal()

print(decision_boundary_plot)

# Note: Visualization depends on the dimensionality of your data and may require PCA or other techniques

# (e) Observations near and far from the Decision Boundary
# (f) Sensibility of Decision Boundary
# These steps involve manual inspection and interpretation based on the visualization.