## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Write a short comment describing this function
## The following function creates a "matrix" object that can cache its inverse.
## it also supports setting matrix, getting matrix, setting inverse and getting inverse.
##The following steps utilized:
## 1. initialize the cache Matrix 'inv'.
## 2. define the method named 'set'.
## 3. define the method named 'get'.
## 4. define the method named 'setinv'.
## 5. define the method named 'getinv'.
## 6. list the names of all methods.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the "matrix" and returned by makeCacheMatrix above. If the inverse has already
## been calculated, then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("loading cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
