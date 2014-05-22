## These two functions below are used to cache the inverse of a square matrix, 
## so every time the same inverse is required, it doesn't need to be recomputed.

## This function creates a special matrix which 
## contains a list of the following functions:
## - set, to set the value of the matrix
## - get, to get the value of the matrix
## - setinverse, to set the value of the inverse of the matrix
## - getinverse, to get the value of the inverse of the matrix

makeCacheMatrix<- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates and returns the inverse of the special matrix that 
## it was created with the above function.
## If the inverse of the matrix has previously calculated, this function will return
## directly the value stored in the cache of the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}
       

