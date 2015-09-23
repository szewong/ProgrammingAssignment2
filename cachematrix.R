## These 2 functions work together to provide caching feature
## for inverse matrix (aka solve).
##

## The makeCacheMatrix returns a list which represents the
## original matrix (x) and it's cached inverse matrix value
## This function itself does not calculate the inverse matrix.
## It provides the data structure for caching the pre-calculated
## inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  #im, as in inverseMatrix, is used to store the cache value.
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) im <<- inverseMatrix
  getInverseMatrix <- function() im
  
  #return the list.
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}



## The cacheSolve function extends from solve and provide
## caching. If x (the matrix) is not changed, it will not re-calculate
## the inverse. It only call solve() initially and will cache the
## result in the list created by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverseMatrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  # Actually calculating the inverse, if rm is NULL.
  im <- solve(data, ...)
  x$setInverseMatrix(im)
  im
}
