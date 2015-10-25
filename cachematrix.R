## This function creates a special "matrix" object that can cache its inverse.
## Actually produces a list containing a function to 1) Set the value of the matrix;
## 2) Get the value of the matrix; 3) Set the value of the inverse matrix; and 4) Get
## the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' from cache.
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  ## Compute a matrix that is the inverse of 'x'.
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
