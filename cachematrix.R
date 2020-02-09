## Put comments here that give an overall description of what your
## functions do
##
## These following functions will calculate and cache a matrix
## and inverts it, to avoid having to recompute it.

## Write a short comment describing this function
##
##In order to save time and energy, you can cache the value of a mean so
## when it is needed again, it can be looked up in the cache instead
## of recomputing it. It will create a matrix that can be 
##inverted when solved.

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
##
##This function 'solves' and calculated the inverse of the above
##computed matrix. If it has previously been cached, then it will
## be retrieved, if the same.

cacheSolve <- function(x, ...) {
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
        ## Return a matrix that is the inverse of 'x'

