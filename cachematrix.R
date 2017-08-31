## These functions will invert a matrix and cache the inverse matrix data

## This function creates the cached matrix and functions for getting and setting the data
makeCacheMatrix <- function(x = matrix()) {
  inverseCache <- NULL
  set <- function(y){
    x <<- y
    inverseCache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseCache <<- inverse
  getInverse <- function() inverseCache
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Check to see if there is a cached inverse and returns it
## if there is no cached matrix,it calculates one and saves it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverse()
  if(!is.null(inverseM)){
    message("getting cached inverse matrix")
    return(inverseM)
  }
  
  data <- x$get()
  inverseM <- solve(data, ...)
  x$setInverse(inverseM)
  inverseM
}

