## Datastructure and function for performing, caching, and retrieving
## the inverse of a a matrix.

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
## and relies upon the cached inverse value on subsequent attempts.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  # If there is an cached inverse indicate that a cached value is 
  # being returned and return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If there is no cached value, get the matrix, solve for its
  # inverse, store the inverse in the data structure, then
  # return the fresh inverse.
  mtx <- x$get()
  m <- solve(mtx, ...)
  x$setinv(m)
  m
}
