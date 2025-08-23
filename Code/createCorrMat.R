createCorrMat <- function(dataSet){
  
  "
  Creates and displays a correlation matrix to investigate relationships between numerical variables in the dataset.
  
  Parameters:
  -----------
  dataSet : data.frame or matrix
      The dataset containing variables to analyze correlation.
  
  Returns:
  --------
  None
      Displays a correlation matrix plot.
  "
  
  corr_matrix <- cor(dataSet %>% select(where(is.numeric)), use = "complete.obs")
  corrplot(corr_matrix, method = "circle")
  mtext("Correlation Matrix", side = 3, line = 2, cex = 1.5, col = "black")
}
