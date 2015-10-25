## This function creates a special Matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function( matrix ) {
        x <<- matrix
        inv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
        inv <<- inverse
  }
    
  getInverse <- function() {
        inv
  }
  list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}




## This function calculates the inverse of the special Matrix created by the above
## function. If the inverse has already been calculated then it retrieves the
## inverse from the cache.


cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
              message("getting cached data")
             return(inv)
  }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
