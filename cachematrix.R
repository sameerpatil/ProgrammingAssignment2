# Since matrix inversion is costly computation, the following functions 
# cache the inverse of matrix, to avoid re-calculation if already available


# This function creates "matrix" object that can cache its inverse 
# using lexical scoping
makeCacheMatrix <- function(x, ...) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

# This function computes the inverse of the matrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  dat <- x$get()
  inverse <- solve(dat, ...) #call the R library function "solve" which does inverse
  x$setInverse(inverse)
  inverse
}