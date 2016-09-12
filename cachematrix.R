## These functions enable you to store the inverse of a matrix in cache so that the inverse of a matrix
## does not need to be calculated repeatedly

## makeCacheMatrix takes a matrix x and returns a list of 4 functions that set x, get x,
## set the inverse of x, and get the inverse of x if it has been calculated by the cacheSolve function
## m holds the inverse of matrix x in memory outside of the global environment.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inversematrix) m<<-inversematrix
  getinverse<-function() m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve calculates sets m (held in memory outside of the global environment) equal to the
## inverse of x if the inverse of x has not already been calculated or set

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
}
