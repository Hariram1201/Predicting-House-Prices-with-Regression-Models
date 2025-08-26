gradBoost <- function(trainMatrix, trainLabel, testMatrix){
  
  #Function that uses the XGBoost model to determine the house prices. 
  #
  # Input Parameter: - trainMatrix: matrix containing 80% of the initial data used to train the models
  #                  - trainLabel: stores the sale prices of the house of the training data
  #                  - testData: matrix containing 20% of the initial data used to test the models
  #
  # Output Parameter: - - predictions: data frame with the sales price predicted by the XGBoost model
  
  # Define parameter grid for tuning
  gbm_grid <- expand.grid(
    n.trees = c(100, 200),              # Number of trees
    interaction.depth = c(4 ,6,8),      # Maximum depth of each tree
    shrinkage = c(0.01, 0.05, 0.1),     # Learning rate (shrinkage)
    n.minobsinnode = c(10, 20)         # Minimum number of observations in each terminal node
  )
  
  # Cross-validation control
  gbm_control <- trainControl(
    method = "cv",       # Cross-validation method
    number = 5,          # Number of folds
    verboseIter = TRUE,  # Show iteration progress
    allowParallel = TRUE # Allow parallel processing
  )
  
  # Train the Gradient Boosting model using caret
  gbm_tuned <- train(
    x = trainMatrix,        # Training data (features)
    y = trainLabel,         # Target variable (SalePrice)
    method = "gbm",         # Gradient Boosting Method
    trControl = gbm_control,  # Cross-validation control
    tuneGrid = gbm_grid,     # Hyperparameter grid
    verbose = TRUE           # Print progress
  )
  
  # View best hyperparameters from tuning
  print(gbm_tuned$bestTune)
  print(plot(gbm_tuned))
  
  # Make predictions on the test data
  predictions <- predict(gbm_tuned, testMatrix)
  
  return(predictions)
}