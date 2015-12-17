rankhospital <- function(state, outcome, num = "best")
{
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
    if(num == "best")
    {
      HN<-odata[which(odata[,col] == min(odata[,col])), ]$Hospital.Name
    }
    else if (num == "worst")
    {
      HN<-odata[which(odata[,col] == max(odata[,col])), ]$Hospital.Name
    }
    else
    {    
      HN<-odata[order(odata[,col],odata$Hospital.Name),]
      HN<-HN[as.numeric(num),]$Hospital.Name
    }
    return(HN)
  }
}