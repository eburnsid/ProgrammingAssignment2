## Together these functions create a matrix with the ability to cache
## its own inverse and then return that inverse, either by getting
## the cached version or, if none exists, calculating it and caching
## for further use.

## The makeCacheMatrix function creates a list of four functions that allow 
## one to get and set both a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function uses a cache matrix created with the previous 
## function to either get the matrix inverse or, if it has not yet been 
## calculated, to find it and both cache it for further use and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
