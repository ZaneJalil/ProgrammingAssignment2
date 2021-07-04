## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() x
  setinverseMatrix <- function(inverseMatrix) i <<- inverseMatrix
  getinverseMatrix <- function() i
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverseMatrix = setinverseMatrix,
       getinverseMatrix = getinverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverseMatrix()
  if (!is.null(i)) {
    message("getting cached data for Matrix")
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverseMatrix(i)
  i
}
