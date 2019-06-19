rankhospital <- function(state, outcome, num = "best"){
  fullData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  subData <- fullData[,c(2,7,11,17,23)]
  names(subData) <- c("hospitalName", "states", "heart attack", "heart failure", "pneumonia")
  if (!outcome %in% names(subData[3:5])){stop("invalid outcome")}
  if (!state %in% subData[,2]) {stop("invalid state")}
  
  stateData <- subset(subData, subData$states == state)
  colIndex <- which(names(stateData)==outcome)
  stateData[,colIndex] <- as.numeric(stateData[,colIndex])
  sorted <- stateData[order(stateData$hospitalName),]
  outcomeSorted <- sorted[order(sorted[,colIndex]),]
  lengthV <- outcomeSorted[,colIndex]
  withoutNA <- lengthV[!is.na(lengthV)]
  n <- length(withoutNA)
  if(num =='best') {result <- outcomeSorted[1,1]}
  else if(num =='worst'){ result <- outcomeSorted[n,1]}
  else if(num > n) {result <- NA}
  else {result <- outcomeSorted[num,1]}
  result
}