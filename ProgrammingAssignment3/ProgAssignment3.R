best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
}