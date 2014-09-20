## makeCacheMatrix takes matrix and initializes internal structure
## to cache the inverse of the matrix, as calculating the inverse is computationally expensive
## cacheSolve stores the inverse internally & retrieves it for future calls

## makeCacheMatrix creates an object that stores the matrix
## and caches the inverse of the matrix.
## Internal structure creates functions that get & set the inverse of the matrix
##
## Input is the matrix for which inverse needs to be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## function that resets matrix and resets cached inverse to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvM <- function(invM) m <<- invM
  getinvM <- function() m
  list(set = set, get = get,
       setinvM = setinvM,
       getinvM = getinvM)
}

## cacheSolve returns the inverse matrix of 
## 'x', which is created via makeCacheMatrix
## if the inverse
cacheSolve <- function(x, ...) {

  ## retreive cached value of the inverse and check if it is derived earlier
  m <- x$getinvM()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## since cached value is NULL, get original data, calculated the inverse
  data <- x$get()
  ## solve is derive the inverse. Other methods may also be used e.g. MASS::ginv
  m <- solve(data)
  ## cache the inverse matrix in the structure for future calls
  x$setinvM(m)
  m
}
