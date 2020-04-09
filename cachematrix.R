## These functions perform matrix inversion on a supplied matrix either
## through calculation via the "solve" function or by returning a cached inverse.

## This first function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This second function computes the inverse of the previously created
## matrix above. If an inverse has been previously calculated, this function
## will instead retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
