rankall <- function(outcome, num = "best") {
  library(plyr)
  options(warn=-1)
  odata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  if (!(outcome %in% c("heart attack","heart failure","pneumonia")))
  {
    stop("invalid outcome")
    
  }
  
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
  odata[,col]<-(as.numeric(odata[,col]))
  odata<-odata[!(is.na(as.numeric(odata[,col]))),]
  odata<-odata[order(odata$State,odata[,col],odata$Hospital.Name),]
  if(num == "best")
  {
    num<-1
    HN<-ddply(odata, .(State), function(x, n) x[n,]$Hospital.Name, n=as.numeric(num))
  }
  else if (num == "worst")
  {
    num<-1
    HN<-ddply(odata[order(odata$State,-odata[,col],odata$Hospital.Name),], .(State), function(x, n) x[n,]$Hospital.Name, n=as.numeric(num))
  }
  else
  {    
    HN<-ddply(odata, .(State), function(x, n) x[n,]$Hospital.Name, n=as.numeric(num))
  }
  HN <- HN[,c(2,1)]
  names(HN)<-c("hospital","state")
  return(HN)
}