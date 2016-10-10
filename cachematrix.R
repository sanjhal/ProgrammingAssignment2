## Matrix inverse calculation is time consuming, thus it is better to cache the
## inverse and use it when required which is done in the following two functions

## This function caches the inverse of the matrix in the variable "inverse". It
## returns a list containing the results of the set, get ,setinverse and
## getInverse functoins.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function  checks if inverse has been calculated and cached. It  displays
## the inverse from the cache if inverse is not null else it calculates the
## inverse and sets the cache.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}