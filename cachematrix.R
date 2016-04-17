## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Functions does the following:
# 1) Setting cached matrix
# 2) Getting cached matrix
# 3) Setting inverse of matrix
# 4) Getting inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse # set inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
# Check cached matrix,
# if solved, get cached matrix
# else, solve inverse of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}