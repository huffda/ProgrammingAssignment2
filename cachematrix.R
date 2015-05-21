## This file contains two R functions that together calculate the 
## inverse of a matrix. It is assumed the matrix submitted to the
## first function is invertible. 



## This function creates and returns a "closure" for a matrix
## and it's inverse, which is assumed to exist. The closure is
## a list of functions for working with the matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(A) {
    x <<- A
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of a matrix (it is
## assumed the inverse exists). If the inverse has not previously
## been calculated, then it is calculated, cached, and returned.
## Otherwise, the cached value is returned without any
## further calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
}
