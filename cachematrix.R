## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
