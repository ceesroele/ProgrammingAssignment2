## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation.
## The functions below provide a wrapper around matrix objects to retrieve a
## cached version of the inverted matrix if available in order to avoid
## costly unnecessary calculations.
## This works only for square but non-singlar matrices.
## Example usage:
## > p <- matrix(sample(9), nrow=3, ncol = 3)
## > q <- makeCacheMatrix(p)
## > cacheSolve(q)  # Inverted matrix is displayed
## > cacheSolve(q)  # Displays "getting cached data" and then inverted matrix

## Write a short comment describing this function
## Create a special "matrix" object that can cache its inverse.
## The argument x is a square but non-singular matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Compute the inverse of the special "matrix".
## The argument x is a wrapper around a square non-singular matrix created 
## with makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
