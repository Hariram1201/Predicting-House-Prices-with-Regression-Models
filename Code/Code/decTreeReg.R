decTreeReg <- function(trainData, testData) {
  
  #Calls a function to use the decision tree regressor model to determine the house prices. 
  #
  # Input Parameter: - trainData: data frame containing 80% of the initial data used to train the models
  #                  - testData: data frame containing 20% of the initial data used to test the models
  #
  # Output Parameter: - predictions: data frame with the sales price predicted by the model
  
  # Set up training control with 10-fold cross-validation
  train_control <- trainControl(method = "cv", number = 10)
  
  # Define a grid of hyperparameters to search
  grid <- expand.grid(
    cp = seq(0.001, 0.0500, by = 0.0050)  
  )
  
  # Train the model using caret with grid search
  treeModel <- train(
    SalePrice ~ ., 
    data = trainData,
    method = "rpart",
    trControl = train_control,
    tuneGrid = grid
  )
  
  # Plot the decision tree
  rpart.plot(treeModel$finalModel, type = 2, extra = 101, fallen.leaves = TRUE,
             main = "Tuned Decision Tree")
  
  # Plot the cross-validation results for each cp value
  # Extract the results of the grid search
  results <- treeModel$results
  
  # Plot RMSE vs CP (complexity parameter)
  plot1 <- ggplot(results, aes(x = cp, y = RMSE)) +
    geom_line() +
    geom_point() +
    labs(title = "RMSE vs CP for Decision Tree Model",
         x = "Complexity Parameter (cp)",
         y = "Root Mean Squared Error (RMSE)") +
    theme_minimal()
  
  # Print the plot explicitly
  print(plot1)  
  
  # Get the best cp value
  bestCp <- treeModel$bestTune$cp
  cat("The best cp value chosen by grid search is:", bestCp, "\n")
  
  # Predict on the test data
  predictions <- predict(treeModel, newdata = testData)
  
  # Return predictions and best model info
  return(predictions)
}