## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#####
# This function intializes two objects, x as a matrix and m as null
# these are then set with an object from the parent environment as y and null, respectively
#   this allows assignment of x from parent environment and clearing of m
# this allows matrix x to be retrieved from the parent environment via get
# setMatrix then retrieves solution solve from the parent environment as a cache
# getMatrix then sets m as the matrix from the parent environment as a cache
# these four functions are all returned within a list to the parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Write a short comment describing this function
#####
# use the function getMatrix to set x as m
#   m originally set as NULL in makeCacheMatrix
# check to see if x stored in cache with if(!is.null(m))
# if stored in cache, return solved matrix
# otherwise, continue the by getting the matrix, solving, adding to the cache via
#   the $set function, and returning the inverted matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
