## Together, these two functions make it possible to cache the inverse of matrices
## without do loops. This can save a lot of time and money by making the process
## more efficient.

## Creates a "special matrix" than can have its inverse cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates the inverse of the special matrix created by makeCacheMatrix
## NOTE: If the inverse is already calculated, the function will retrieve it
## from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

        ## Return a matrix that is the inverse of 'x'
}
