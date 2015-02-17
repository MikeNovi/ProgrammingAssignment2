## Datastructure and function for performing, caching, and retrieving
## 

## Creates a datastructure that contains a matrix, cached inverse,
## and accessors for getting/setting the inverse value.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function using the makeCacheMatrix to 'solve' for the inverse
## of a supplied matrix. Calculates the inverse on first attempt 
## and relies upon the cached inverse value on ubsequent attempts.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mtx <- x$get()
  m <- solve(mtx, ...)
  x$setinv(m)
  m
}
