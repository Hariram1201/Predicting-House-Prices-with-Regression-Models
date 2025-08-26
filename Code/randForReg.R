randForReg <- function(trainData, testData){
  
  #Function that uses the random forest regressor model to determine the house prices. 
  #
  # Input Parameter: - trainData: data frame containing 80% of the initial data used to train the models
  #                  - testData: data frame containing 20% of the initial data used to test the models
  #
  # Output Parameter: - bestRandForMAE: variable storing the MAE value for the best random forest
  #                                     regressor model
  #                   - bestRandForRMSE: variable storing the RMSE value for the best random forest
  #                                      regressor model
  #.                  - bestRandForRSqr: variable storing the R Squared value for the best random forest
  #                                     regressor model
  
  # Create a tuning grid for the mtry parameter (Random Forest parameter)
  tuneGrid <- expand.grid(mtry = c(1:20))  # Specify different values for mtry
  
  # Define cross-validation settings
  trainControl <- trainControl(
    method = "cv",           # Cross-validation method
    number = 5,              # Number of folds
    search = "grid"          # Grid search method
  )
  
  #Stores the evaluation metrics for each of the different ntree values
  nTreeResult <- data.frame(matrix(NA, nrow = 3, ncol = 5))
  x <- 1
  
  results_df <- data.frame()
  
  # Train the Random Forest model using grid search for mtry
  for (ntree in c(50, 100, 200)){
    rfModel <- train(
      SalePrice ~ .,            # Formula specifying target variable
      data = trainData,    # Training dataset
      method = "rf",            # Random Forest method
      trControl = trainControl,  # Cross-validation settings
      tuneGrid = tuneGrid,     # Grid for mtry parameter
      ntree = ntree,              # Number of trees in the forest
      nodesize = 5,             # Minimum size of terminal nodes
      importance = TRUE         # Track variable importance
    )
    
    # Print the best tuning results
    print(rfModel)
    
    # Make predictions on the test data
    prediction <- predict(rfModel, newdata = testData)
    
    #Calls a function to evaluate the decision tree regressor model 
    modelEvaluation <- evaluateModel(testData, prediction, meanSalePrice)
    
    #Extracts the MAE, RMSE and R^2 values from the list
    randForMAE <- as.data.frame(modelEvaluation[1])
    nTreeResult[1,x] <- randForMAE
    randForRMSE <- as.data.frame(modelEvaluation[2])
    nTreeResult[2,x] <- randForRMSE
    randForRSqr <- as.data.frame(modelEvaluation[3])
    nTreeResult[3,x] <- randForRSqr
    
    x <- x + 1
    
    rfModel$results$ntree <- ntree  # Add ntree info to results
    results_df <- rbind(results_df, rfModel$results)  # Store all results
  }
  
  # Find the largest R^2 value (best model)
  largestRSqr <- which.max(nTreeResult[3, ])
  
  # Get the corresponding ntree value for the best R^2
  bestnTree <- c(50, 100, 200)[largestRSqr]
  
  # Extract the MAE, RMSE, and R^2 for the best model
  bestRandForMAE <- nTreeResult[1, largestRSqr]
  bestRandForRMSE <- nTreeResult[2, largestRSqr]
  bestRandForRSqr <- nTreeResult[3, largestRSqr]
  
  plot1 <- ggplot(results_df, aes(x = mtry, y = RMSE, color = as.factor(ntree))) +
    geom_line() +
    geom_point() +
    labs(title = "Effect of mtry and ntree on RMSE in Random Forest",
         x = "mtry (Number of Features Tried at Each Split)",
         y = "Root Mean Squared Error (RMSE)",
         color = "Number of Trees (ntree)") +
    theme_minimal()
  
  # Print the plot explicitly
  print(plot1) 
  
  return(list(bestRandForMAE, bestRandForRMSE, bestRandForRSqr, prediction))
}