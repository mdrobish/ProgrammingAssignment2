## Calculating inverse of a matrix can be time consuming.  By caching the calculated matrix inverse,
## we can improve the performance of the repeated requests.  We can achieve caching by using the following
## two functions.
## 
## 

## makeCacheMatrix
## This function takes a squared matrix and returns a list of get and set functions.
## They provide the functions to get and set calculated and cached inverse matrix
## Use this function first to create a cacheMatrix functions.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <-function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <-function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <-function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve
## This function returns cached matrix inverse if exists.  If not, it calculates the matrix inverse, 
## and saves to cache before returning the alcutes inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
