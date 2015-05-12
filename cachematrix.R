## The first function, makeCacheMatrix creates a list
## containing functions to get/set the matrix and
## get/set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newm){
    x <<- newm
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(new_inv) inverse <<- new_inv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function  calculates the inverse of matrix and caches 
## the result. If the cache already contains a valid inverse, the 
## cached version is returned.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
      message("getting cached results")
      return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}

## Helpful commands for testing:
## (non-singularity is not guaranteed,
##  but very unlikely)
##
## m <- matrix(runif(16),4)
## res <- makeCacheMatrix(m)
## cacheSolve(res)
## cacheSolve(res)
