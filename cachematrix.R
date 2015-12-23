## This R function is able to cache potentially time-consuming computations.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  CI<-NULL
  set<-function(y) {
    x<<-y
    CI<<-NULL
  }
  get<-function() x
  SI<-function(inv) CI<<-inv
  GI<-function() CI
  list(set=set,get=get,SI=SI,GI=GI)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  CI<-x$GI()
  if(!is.null(CI)) {
    message("getting cached data")
    return(CI)
  }
  data<-x$get()
  CI<-solve(data,...)
  x$SI(CI)
  CI
}
