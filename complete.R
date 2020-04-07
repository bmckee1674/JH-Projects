complete <- function(directory, id = 1:332){
  files_list <- list.files(directory, full.names = TRUE)
  df <- data.frame()
  nobs <- numeric()
  for (i in id) {
    df<- read.csv(files_list[i])
    nobs<- c(nobs, sum(complete.cases(df) == TRUE))
  }
  output <- data.frame(id, nobs)
  output
}