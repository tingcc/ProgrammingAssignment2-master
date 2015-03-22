## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix
##  1. cache the original matrix
##  2. cache the inversed matrix
##  3. return 4 functions in a list
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## check if the inverse already exist in cache.  If so, get cached solution,
## otherwise perform calculation of the matrix inverse anc cache it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
