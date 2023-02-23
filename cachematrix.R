## Combination of functions makeCacheMatrix and cacheSolve allows calculate
## inverse matrix only one times for each matrix. Second and next requests 
## for inverse matrix are answered from cache.This solution save time.

## Function makeCacheMatrix creates object that can cache inverse matrix
## after calculating for next use.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Check if the matrix has cached solution and return if there is one.
## Otherwise solve the matrix, store the solution in cache and return the solution.

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- inv(data, ...)
  x$setinv(inverse)
  inverse
}

## Return a matrix that is the inverse of 'x'
