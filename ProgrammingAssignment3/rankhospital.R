rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  ## Check that state and outcome are valid
  possibleOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  validState = state %in% data$State
  validOutcome = outcome %in% possibleOutcomes
  
  if (isFALSE(validState)) {
    stop("invalid state")
  }
  
  if (isFALSE(validOutcome)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  data <- data[which(data$State == state), ]
  
  if (outcome == 'heart attack'){
    data <- data[which(!is.na(data[, 11])), ]
    data <- data[order(data[, 11], data[, 2]), ]
  } else if (outcome == 'heart failure'){
    data <- data[which(!is.na(data[, 17])), ]
    data <- data[order(data[, 17], data[, 2]), ]
  } else {
    data <- data[which(!is.na(data[, 23])), ]
    data <- data[order(data[, 23], data[, 2]), ]
  }
  
  ## 30-day death rate
  if (num == "best") {
    return(data$Hospital.Name[1])
  } else if (num == "worst") {
    return(data$Hospital.Name[nrow(data)])
  } else
    return(data$Hospital.Name[num])
}
