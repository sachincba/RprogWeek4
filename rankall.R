rankall <- function(outcome, num='best'){
  fullData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  subData <- fullData[,c(2,7,11,17,23)]
  names(subData) <- c("hospitalName", "states", "heart attack", "heart failure", "pneumonia")
  if (!outcome %in% names(subData[3:5])){stop("invalid outcome")}
  colIndex <- which(names(subData)==outcome)
  subData[,colIndex] <- as.numeric(subData[,colIndex])
  subData = subData[!is.na(subData[,colIndex]),]
  subData = subData[order(subData[,2],subData[,colIndex],subData[,1]),]
  listed <- split(subData, subData[,2])
  statelist <- lapply(listed, function(x) x[1,2])
  if(num=='best'){ 
    hosplistbest <- lapply(listed, function(x) x[1,1])
    result <- data.frame(hospital = unlist(hosplistbest), state = unlist(statelist))
  }
  else if(num=='worst'){ 
    hosplistworst <- lapply(listed, function(x) x[nrow(x),1])
    result <- data.frame(hospital = unlist(hosplistworst), state = unlist(statelist))
  }
  else {
    hosplist <- lapply(listed, function(x) x[num,1])
    result<- data.frame(hospital = unlist(hosplist), state = unlist(statelist))
  }
  return(result)
}
