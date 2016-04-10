## Makes a matrix that can easily cache its inverse.

## Makes a matrix that creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inverse) {
    inv <<- inverse
  }
  getinv <- function() {
    inv
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the matrix
## Uses cached value if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
