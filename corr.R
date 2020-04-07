corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names = TRUE)
  df <- data.frame()
  correlation <- numeric()
  
  output <- complete(directory)
  output
  
  extract <- subset(output, nobs > threshold)
  
  rownames(extract) = NULL
  
  id <- extract[,"id"]
  
  for (i in id) {
    df <- read.csv(files_list[i])
    correlation <- c(correlation, cor(df[,"nitrate"],df[,"sulfate"], use = "complete.obs"))
  }
  correlation
}