rankall <- function(outcome, num = "best") {
  ##Read outcome data
  hospitals <-  read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available", stringsAsFactors = FALSE)
  hospitals_2 <- hospitals[,c(2,7,11,17,23)]
  names(hospitals_2) <- c("hospital", "states", "heart attack", "heart failure", "pneumonia")
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ##Check that outcome is valid
  if(!(outcome %in% outcomes)){
    stop("invalid outcome")
  }
  ##Return a data frame with the hospital names and the state names
  if(num == "best"){
    hospitals_3 <- hospitals_2[,c("hospital", "states", outcome)]
    names(hospitals_3) <- c("hospital", "states", "outcomes")
    hospitals_4 <- hospitals_3
    hospitals_4 <- na.omit(hospitals_4)
    
    hospitals_4 <- transform(hospitals_4, outcomes = as.numeric(outcomes))
    order_hospitals <- hospitals_4[order(hospitals_4$states, hospitals_4$outcomes, hospitals_4$hospital),]
    
    split_states <- split(order_hospitals, order_hospitals$states)
    list_hospitals <- unlist(lapply(split_states, function(x) x[1,1]))
    list_states <- unlist(lapply(split_states, function(y) y[1,2]))
    
    output <- data.frame(hospital = list_hospitals, state = list_states)
    
  } else if(num == "worst"){
    hospitals_3 <- hospitals_2[,c("hospital", "states", outcome)]
    names(hospitals_3) <- c("hospital", "states", "outcomes")
    hospitals_4 <- hospitals_3
    hospitals_4 <- na.omit(hospitals_4)
    
    hospitals_4 <- transform(hospitals_4, outcomes = as.numeric(outcomes))
    order_hospitals <- hospitals_4[order(hospitals_4$states,-hospitals_4$outcomes, hospitals_4$hospital),]
    
    split_states <- split(order_hospitals, order_hospitals$states)
    list_hospitals <- unlist(lapply(split_states, function(x) x[1,1]))
    list_states <- unlist(lapply(split_states, function(y) y[1,2]))
    
    output <- data.frame(hospital = list_hospitals, state = list_states)
    
  } else {
    hospitals_3 <- hospitals_2[,c("hospital", "states", outcome)]
    names(hospitals_3) <- c("hospital", "states", "outcomes")
    hospitals_4 <- hospitals_3
    hospitals_4 <- na.omit(hospitals_4)
    
    hospitals_4 <- transform(hospitals_4, outcomes = as.numeric(outcomes))
    order_hospitals <- hospitals_4[order(hospitals_4$outcomes, hospitals_4$hospital),]
    output <- character()
    
    rownames(order_hospitals) <- NULL
    output <- order_hospitals[num,"hospital"]
    
    split_states <- split(order_hospitals, order_hospitals$states)
    list_hospitals <- unlist(lapply(split_states, function(x) x[num,1]))
    list_states <- unlist(lapply(split_states, function(y) y[1,2]))
    
    output <- data.frame(hospital = list_hospitals, state = list_states)
    
  }
  
  output
  
}