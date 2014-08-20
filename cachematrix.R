## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix(): create an augmented matrix object that caches its
##   inverse.
## return a list of set/get/setsolve/getsolve functions to manipulate it
## cache inverted matrix

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


## Write a short comment describing this function
## cacheSolve(): invert a matrix, and cache its values. The next time the 
##   matrix request inversion, it cache value will be returned.
## return the inverted matrix as normal
## this function should be called with a special matrix return by makeCacheMatrix()

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
