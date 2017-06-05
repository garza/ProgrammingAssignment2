## Programming Assignment 2 for Coursera - R Programming (Week 2)

## implement a pair of functions that cache the inverse of a matrix.

## Based off of makeVector from Programming Assignment 2 instructions
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Based off of cacheMean from Programming Assignment 2 instructions
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  computedInverse <- x$getinverse()
  if(!is.null(computedInverse)) {
    message("getting cached data")
    return(computedInverse)
  }
  data <- x$get()
  computedInverse <- solve(data)
  x$setinverse(computedInverse)
  computedInverse
}
