## Put comments here that give an overall description of what your
## functions do

## This function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  h <- NULL
  set <- function(y) {
    x <<- y
    h <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) h <<- Inverse
  getInverse <- function() h
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by the function above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  v <- x$getInverse()
  if(!is.null(v)) {
        message("getting cached data")
        return(v)
  }
  mat <- x$get()
  v <- solve(mat, ...)
  x$setInverse(v)
  v
}

