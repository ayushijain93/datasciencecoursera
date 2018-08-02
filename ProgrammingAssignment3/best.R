best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  possibleOutcomes <- c('heart attack', 'heart failure', 'pneumonia')

  ## Check that state and outcome are valid
  validState = state %in% data$State
  validOutcome = outcome %in% possibleOutcomes
  
  if (isFALSE(validState)) {
    stop("invalid state")
  }
  
  if (isFALSE(validOutcome)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  data <- data[which(data$State == state), ]
  data <- data[order(data$Hospital.Name), ]
  
  if (outcome == 'heart attack'){
    lowestDeathRate = which.min(data[, 11])
  } else if (outcome == 'heart failure'){
    lowestDeathRate = which.min(data[, 17])
  } else {
    lowestDeathRate = which.min(data[, 23])
  }
  
  hospitalName <- data[lowestDeathRate, 2]
  return(hospitalName)
}