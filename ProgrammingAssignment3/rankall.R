rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  possibleOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  validOutcome = outcome %in% possibleOutcomes
  
  if (isFALSE(validOutcome)) {
    stop("invalid outcome")
  }
  
  if (outcome == 'heart attack'){
    data[, 11] <- as.numeric(data[, 11])
    data <- data[which(!is.na(data[, 11])), ]
    data <- data[order(data[, 11], data[, 2]), ]
  } else if (outcome == 'heart failure'){
    data[, 17] <- as.numeric(data[, 17])
    data <- data[which(!is.na(data[, 17])), ]
    data <- data[order(data[, 17], data[, 2]), ]
  } else {
    data[, 23] <- as.numeric(data[, 23])
    data <- data[which(!is.na(data[, 23])), ]
    data <- data[order(data[, 23], data[, 2]), ]
  }
  
  databystate <- split(data, data$State)
  
  if (num == "best") {
    hospital <- sapply(databystate, function(x){x$Hospital.Name[1]})
  } else if (num == "worst") {
    hospital <- sapply(databystate, function(x){x$Hospital.Name[nrow(x)]})
  } else {
    hospital <- sapply(databystate, function(x){x$Hospital.Name[num]})
  }
  
  return(as.data.frame(hospital))
}
