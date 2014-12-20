## The functions create a special matrix and caches its inverse 

## The function makeCacheMatrix creates a special matrix that takes
## a numeric matrix as input and outputs a special matrix that is
## a list of functions that allow the special matrix to cache the 
## numeric matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## function to store the input numeric matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  ## function to retrieve the input matrix
  get <- function() x
  ## function to retrieve the inverse of the input matrix
  getinv <- function() inverse
  ## function to cache the inverse
  setinv <- function(inv) inverse <<- inv
  ## list of functions that make the numeric matrix a special matrix
  list(set=set, get=get, setinv=setinv, getinv= getinv)
}


## The cacheSolve function takes the special matrix created
## by the above function as input and finds the inverse of the 
## special matrix and caches it for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  ## Checks if the inverse exists in cache
  if (!is.null(inverse)){
    message("getting cahced data")
    return(inverse)
  }
  ## If no inverse then compute one
  data <- x$get()
  inverse <- solve(data)
  ##Cache the inverse of the special matrix
  x$setinv(inverse)
  inverse
}

