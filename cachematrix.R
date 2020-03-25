# This program creates a special "matrix" object that can cache its inverse and computes the inverse of the
# special "matrix". If the inverse has already been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.

#This function:

##sets the value of the matrix
##gets the value of the matrix
##sets the value of the inverse
##gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  cmi <- NULL
  set <- function(y) {
    x <<- y
    cmi <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) cmi <<- inverse
  get_inverse <- function() cmi
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# this function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert <- x$get_inverse()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$set_inverse(invert)
  invert
}