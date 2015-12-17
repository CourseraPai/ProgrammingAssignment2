best <- function(state, outcome){
  
  options(warn=-1)
  odata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  if (!(outcome %in% c("heart attack","heart failure","pneumonia")))
  {
    stop("invalid outcome")
    
  }
  
  else if (!(state %in% unique(odata$State)))
  {
    stop("invalid state")
    
  }
  else
  {
    if(outcome=="heart attack")
    {
      col=11
    }
    else if (outcome=="heart failure")
    {
      col=17
    }
    else if (outcome =="pneumonia")
    {
      col=23
    }
  odata<-odata[odata$State==state,]
  odata[,col]<-(as.numeric(odata[,col]))
  odata<-odata[!(is.na(as.numeric(odata[,col]))),]
  HN<-odata[which(odata[,col] == min(odata[,col])), ]$Hospital.Name
  return(HN)
  }
}