best <- function(state,outcome){
  fullData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  subData <- fullData[,c(2,7,11,17,23)]
  names(subData) <- c("hospitalName", "states", "heart attack", "heart failure", "pneumonia")
  if (!outcome %in% names(subData[3:5])){stop("invalid outcome")}
  if (!state %in% subData[,2]) {stop("invalid state")}
  stateData <- subset(subData, subData$states == state)
  colIndex <- which(names(stateData)==outcome)
  stateData[,colIndex] <- as.numeric(stateData[,colIndex])
  minRate <- min(stateData[,colIndex], na.rm = TRUE)
  rowIndex <- which(stateData[,colIndex]==minRate)
  stateData[rowIndex,1]

}





