rankhospital <- function(state, outcome, num = "best"){
  ##Read outcome data
  hospitals <-  read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", stringsAsFactors = FALSE)
  hospitals_2 <- hospitals[,c(2,7,11,17,23)]
  names(hospitals_2) <- c("hospital", "states", "heart attack", "heart failure", "pneumonia")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ##Check that state and outcome are valid
  if(!(outcome %in% outcomes)){
    stop("invalid outcome")
  }
  if(!(state %in% hospitals_2[,"states"])){
    stop("invalid state")
  }
  
  ##Return hospital name in that state with the given rank
  ##30-day death rate
  if(num == "best"){
    hospitals_3 <- hospitals_2[,c("hospital", "states", outcome)]
    names(hospitals_3) <- c("hospital", "states", "outcomes")
    hospitals_4 <- subset(hospitals_3, hospitals_3$states %in% state, na.omit = TRUE)
    hospitals_4 <- na.omit(hospitals_4)
    
    hospitals_4 <- transform(hospitals_4, outcomes = as.numeric(outcomes))
    order_hospitals <- hospitals_4[order(hospitals_4$outcomes, hospitals_4$hospital),]
    output <- character()
    
    rownames(order_hospitals) <- NULL
    output <- order_hospitals[1,"hospital"]
  } else if(num == "worst"){
    hospitals_3 <- hospitals_2[,c("hospital", "states", outcome)]
    names(hospitals_3) <- c("hospital", "states", "outcomes")
    hospitals_4 <- subset(hospitals_3, hospitals_3$states %in% state, na.omit = TRUE)
    hospitals_4 <- na.omit(hospitals_4)
    
    hospitals_4 <- transform(hospitals_4, outcomes = as.numeric(outcomes))
    order_hospitals <- hospitals_4[order(-hospitals_4$outcomes, hospitals_4$hospital),]
    output <- character()
    
    rownames(order_hospitals) <- NULL
    output <- order_hospitals[1,"hospital"]
  } else {
    hospitals_3 <- hospitals_2[,c("hospital", "states", outcome)]
    names(hospitals_3) <- c("hospital", "states", "outcomes")
    hospitals_4 <- subset(hospitals_3, hospitals_3$states %in% state, na.omit = TRUE)
    hospitals_4 <- na.omit(hospitals_4)
    
    hospitals_4 <- transform(hospitals_4, outcomes = as.numeric(outcomes))
    order_hospitals <- hospitals_4[order(hospitals_4$outcomes, hospitals_4$hospital),]
    output <- character()
    
    rownames(order_hospitals) <- NULL
    output <- order_hospitals[num,"hospital"]
  }
  
  output
}