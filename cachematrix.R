## In this project, I created a set of two functions to store and retrieve a matrix and its inverse from cache

## makeCacheMatrix() creates a list of four functions: 
## set: to store the matrix in the cache
## get: to retrieve the matrix previously stored in the cache
## setInverse: to store the inverse of a matrix in the cache
## getInverse: to retrieve the inverse of a matrix previously stored in the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverser = setInverse,
       getInverse = getInverse)
}

## Calculates the inverse of a matrix (if it does not exist), then calls the function makeCacheMatrix() to 
## store it in the cache and finally returns it. If it does exist, it just retrieve it from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
