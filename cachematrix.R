## The functions create a special matrix and caches its inverse 

## The function makeCacheMatrix creates a special matrix that takes
## a numeric matrix as input and outputs a special matrix that is
## a list of functions that allow the special matrix to cache the 
## numeric matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  getinv <- function() inverse
  setinv <- function(inv) inverse <<- inv
  
  list(set=set, get=get, setinv=setinv, getinv= getinv)
}


## The cacheSolve function takes the special matrix created
## by the above function as input and finds the inverse of the 
## special matrix and caches it for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if (!is.null(inverse)){
    message("getting cahced data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}

