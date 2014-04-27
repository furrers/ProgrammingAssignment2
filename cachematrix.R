## Create an inversable matrix

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. Takes a numeric vector x as input.
makeCacheMatrix <- function(x = matrix()) {
  value <- NULL
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) value <<- solve
  getInverse <- function() value
  # Returns list of wrapped functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Returns the inversed matrix of the special vector x from makeCacheMatrix.
cacheSolve <- function(x, ...) {
  value <- x$getInverse()
  if(!is.null(value)) {
    message("getting cached data")
    return(value)
  }
  data <- x$get()
  value <- solve(data, ...)
  x$setInverse(value)
  value
}
