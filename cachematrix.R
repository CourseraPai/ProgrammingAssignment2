##This is an exercise to depict the difference between the <- and the <<- operators.
##The main point to be noted is when a variable is set with the <<- operator, 
##it acts like a global variable. When that variable is invoked, a serach is first made to see if 
##it has been set in the current loop or function with a <- operator. if it has been set then 
##the variable takes the local value. If not set within current environment whatever has 
##been set with the <<- operator comes into play.


## In this function we set x with a matrix value with a <<- operator, which means 
## this will come into play only if
## x does not get reset with <-operator wherever it is called 

makeCacheMatrix <- function(x = matrix()) {
  library(MASS)
  m <- NULL
  setmatrix <- function(y) {
    x <<- y ## x gets the value y
    print(x) ## this is to prove that within the function when printed will take the value y
    m <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(ginv) m <<- ginv ## use this value of m when m is not set otherwise in the environment which calls setinv
  getinv <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)

}


## Here we find the inverse of a matrix which is 

cacheSolve <- function(x, ...) {
  m <- x$getinv() 
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$getmatrix()
  library(MASS)
  m <- ginv(data, ...)
  x$setinv(m)
  m
}

## Here is an example which can help us understand the <<- operator. Run these in order to understand better
## 1.x<- makeCacheMatrix() , this means x gets set with the output of the function makeCacheMatrix()
## 2.samplematrix<- matrix(c(3,5,2,0),nrow=1,ncol=4) , we create a sample matrix of 1 X 4 dimensions
## 3.x$setmatrix(samplematrix) , as step one has defined a value for x with <- operator, x can use the functions within makeCacheMatrix()
##   But the print statement within print(x) will print y=samplematrix. This means it has taken the value of y within the function though outside it is equal to makeCacheMatrix() due to step1
## 4.x$getmatrix() will print the samplematrix
## 5.cachesolve(x) , calculates inverse of matrix as m
## 6.cachesolve(x) , running cachesolve again will fetch from the cache and not run the function again