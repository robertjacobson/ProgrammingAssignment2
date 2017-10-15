## makeCacheMatrix:
## This function creates a "matrix" object that ## can cache
## its inverse.
##
## caccheSolve:
## receives a cacheMatrix object, returns the inverse.


## receives a matrix (must be invertible), returns an object
## with get/set inverse functions:
##    getInverse - retrieves inverse from memory
##    setInverse - sets the inverse in memory
makeCacheMatrix <- function(matrix = matrix()) {
   inverse <- NULL
   set <- function(y) {
     matix <<- y
     inverse <<- NULL
   }
   get <- function() matrix
   setInverse <- function(solve) inverse <<- solve
   getInverse <- function() inverse
   list(set=set, get=get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse exists in
## cache then that result is returned. Otherwise, the inverse
## is calculated and cached.
cacheSolve <- function(cacheMatrix, ...) {
  cache <- cacheMatrix$getInverse()
  if(!is.null(cache)) {
    message("Cached data found, returning")
    return(cache)
  }
  data <- cacheMatrix$get()
  inv <- solve(data, ...)
  cacheMatrix$setInverse(inv)
  inv
}
