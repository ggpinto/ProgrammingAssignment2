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