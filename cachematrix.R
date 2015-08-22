## Put comments here that give an overall description of what your
## functions do

## This function returns the 'special matrix'
## containing all the parameters defined in
## the role: media and matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmean<-function(mean) m<<- mean
  getmean<-function() m
  list(set=set, get=get,
       setmean=setmean,
       getmean=getmean)
}


## Check if the calculations are made for the entered
## matrix are calculated if not, then use the 'solve'
## function to calculate the inverse.

cacheSolve <- function(x = matrix(), ...) {
  m<-x$getmean()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setmean(m)
  m
}