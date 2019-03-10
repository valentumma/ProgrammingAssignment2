##The function makeCacheMatrix creates a special "vector", which is really a list 
##containing a function to
##  1. set matrix
##  2. get matrix
##  3. setInverse - to set the inverse matrix
##  4. getInverse - to get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(InMatrix) m <<- InMatrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function creats an inverse Matrix. However, it first checks if there is an inverse Matrix 
##already in cache.  If so, it gets the inverse matrix from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
  ##Is checking if there is already an inverse matrix in cache
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)        ##returning cached data
  }
  ##Getting data for calculation
  data <- x$get()
  ## Return a matrix that is the inverse of 'x'
  m <- solve(data, ...) 
  x$setInverse(m)
  m
}
