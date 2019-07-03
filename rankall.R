rankall <- function(outcome, num = "best"){
  #Read outcome data
  data <- read.csv('outcome-of-care-measures.csv', header = TRUE, na.strings="Not Available")
  state <- levels(data[,7])
  ## Check that state and outcome are valid and gave column numbers based on the inputted outcome
  if ((state %in% data[,"State"]) == FALSE) {stop('invalid state')}
  if ((outcome == "heart attack") == TRUE){col1 <- 11}
  else if ((outcome == "heart failure") == TRUE){col1 <- 17}
  else if ((outcome == "pneumonia") == TRUE){col1 <- 23}
  else {stop('invalid outcome')}
  # For each state, find the hosptial of the given rank
  selectedOutcome <- as.numeric(data[, col1])
  
  allstate<-matrix()
  for (i in 1:length(state)){
    sd <- subset(data, State == state[i])
    sc <- sd[,col1]
    sd <- sd[!(is.na(sc)),]
    hrps <-sd[order(sd[, col1], sd[, 2]), ]
      if(num =="best"){num =1}
      else if(num =="worst"){num = nrow(hrps)}
      else{num = num}
    hpr<-as.character(hrps[num,2])
    allstate <- rbind(allstate,c(hpr,state[i]))
  }
  return(allstate)
}
source("rankall.R")
 tail(rankall("pneumonia","worst"))
 head(rankall("heart attack",20))
 tail(rankall("heart failure"),10)
 
