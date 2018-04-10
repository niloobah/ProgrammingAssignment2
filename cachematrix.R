## A pair of functions that cache the inverse of 
## a matrix

## The fuctions are: 
## makeCacheMatrix: This function creates a special
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # return a matrix for calculation of next function
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse matrix
  setinv <- function(inverse) inv <<- inverse
  # get the value of the inverse matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}