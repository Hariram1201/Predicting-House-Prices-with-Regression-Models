xgboost <- function(trainMatrix, trainLabel, testMatrix){
  
  #Function that uses the XGBoost model to determine the house prices. 
  #
  # Input Parameter: - trainMatrix: matrix containing 80% of the initial data used to train the models
  #                  - trainLabel: stores the sale prices of the house of the training data
  #                  - testMatrix: matrix containing 20% of the initial data used to test the models
  #
  # Output Parameter: - - predictions: data frame with the sales price predicted by the XGBoost model
  
  # Define parameter grid for tuning
  xgb_grid <- expand.grid(
    nrounds = c(100, 200),
    max_depth = c(6, 8, 10),
    eta = c(0.01, 0.05, 0.1),
    gamma = c(0,1),
    colsample_bytree = c(0.6, 0.7),
    min_child_weight = 1,
    subsample = c(0.8, 0.9)
  )
  
  # Cross-validation control
  xgb_control <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    allowParallel = TRUE
  )
  
  # Train the model using caret
  xgb_tuned <- train(
    x = trainMatrix,
    y = trainLabel,
    method = "xgbTree",
    trControl = xgb_control,
    tuneGrid = xgb_grid,
    verbose = TRUE
  )
  
  # View best hyperparameters
  print(xgb_tuned$bestTune)
  print(plot(xgb_tuned))
  
  # Make predictions on the test data
  predictions <- predict(xgb_tuned, testMatrix)
  
  return(predictions)
}