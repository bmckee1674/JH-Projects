pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names = TRUE)
  df <- data.frame()
  for (i in id) {
    df<- rbind(df, read.csv(files_list[i]))
  }
  df_subset <- df[, pollutant]
  
  mean(df_subset, na.rm = TRUE)
  
}