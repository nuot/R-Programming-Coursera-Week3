rankhospital <- function (state, outcome, num = "best"){
  ## Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', header = TRUE, na.strings="Not Available")
  State <- levels(data[,7])
  ## Check that state and outcome are valid and gave column numbers based on the inputted outcome
  if ((state %in% data[,"State"]) == FALSE) {stop('invalid state')}
  if ((outcome == "heart attack") == TRUE){col1 <- 11}
  else if ((outcome == "heart failure") == TRUE){col1 <- 17}
  else if ((outcome == "pneumonia") == TRUE){col1 <- 23}
  else {stop('invalid outcome')}
  ## Return hospital name in that state with the given rank of 30-day death rate
  # Get hosptials' name
  data[,2] <- as.character(data[,2])
  # Select specific state by the input
  selectedState <- subset(data, State == state)
  # Select outcome under the selected state by the input
  selectedOutcome <- as.numeric(selectedState[, col1])
  # Omit all NAs in data
  selectedState<- selectedState[!(is.na(selectedOutcome)),]
  # Rank hospital by inputted outcome
  RankedHospital<-selectedState[order(selectedState[,col1]),col1]
  HospitalName <- as.character(selectedState[order(selectedState[,col1]),2])
  if(num == "best") {num = 1}
  else if(num =="worst"){num = length(HospitalName)}
  else {num = num}
  
  return(HospitalName[num])
}
  source("rankhospital.R")
  rankhospital("MD", "heart attack", "worst")
  rankhospital("TX", "heart failure", 4)
  rankhospital("MN", "heart attack", 5000)
 
