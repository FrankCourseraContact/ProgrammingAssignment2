## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix description:
## Accepts x, a square invertible matrix
## setMatrix: Creates a special matrix (x)
## getMatrix: Retrieves the special matrix (x)
## setInverse: Accepts an inverse and sets it to i
## getInverse: Retrieves i
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL

  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list( setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse )
  
}


## cacheSolve description:
## Accepts two arguments: x, a makeCacheMatrix() object, and
## '...' to be passed to solve()
## Gets 'i'. If i = NULL, it caches the inverse.
## If 'i' is NOT NULL, it returns i (the inverse)
cacheSolve <- function(x, ...) {

  i <- x$getInverse()
  if ( !is.null(i) ) {
    message( "Getting Cached Inverse" )
    return (i)
  }
  else { message( "Caching Inverse" )}

  data <- x$getMatrix()
  i <- solve(data)
  x$setInverse(i)
  i
}
