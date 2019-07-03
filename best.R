best <- function (state, outcome){
  #Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', header = TRUE, na.strings="Not Available")
  State <- levels(data[,7])
  #Check that state and outcome are valid
  if ((state %in% data[,"State"]) == FALSE) {stop('invalid state')}
  if ((outcome == "heart attack") == TRUE){col1 <- 11}
  else if ((outcome == "heart failure") == TRUE){col1 <- 17}
  else if ((outcome == "pneumonia") == TRUE){col1 <- 23}
  else {stop('invalid outcome')}
  selectedState <- subset(data, State == state)
  selectedOutcome <- selectedState[,col1]
  selectedOutcome <- na.omit(selectedOutcome)
  selectedRows <- which(selectedOutcome == min(selectedOutcome))
  selectedHospital <- selectedState[selectedRows, 2]
  selectedHospital
}
setwd("/Users/tiannuo/Desktop/R_Coursera/rprog_data_ProgAssignment3-data")
source("best.R")
best("AK", "heart attack")
best("CB","heart attack")
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
