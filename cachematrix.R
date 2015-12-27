## The following two functions work together cache the inverse of a matrix and then display as requested 

## makeCacheMatrix is a helper function that creates a list of four functions.  These functions can
## then be used later in cacheSolve when caching the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve does the work of caching or retrieving the matrix inverse.  if m (the inverse) is null, 
## that means the function is being run for the first time and this function needs to call the methods of 
## makeCacheMatrix to compute the inverse and store it.  Otherwise, it uses the stored value of m.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
