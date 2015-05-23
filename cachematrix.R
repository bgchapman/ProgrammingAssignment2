## Functions that create a special "matrix" object that 
## stores a numeric matrix and caches its inverse.

## Creates the "matrix" object as a list containing a function to
## set and get the value of the matrix and to set and get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates and returns a matrix that is the inverse of 'x'.  Uses a cached inverse if it exists,
## otherwise calculates the inverse via the solve function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
