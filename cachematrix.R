## A pair of functions that cache the inverse of a matrix
# Naim Jalil- Coursera Programming Assignment number 2

## Creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
    i <- NULL
    
    
  # Set the Matrix
    setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get the Matrix
  getMatrix <- function() x
  
  #set the Inverse Matrix
  setinverseMatrix <- function(inverseMatrix) i <<- inverseMatrix
  
  #get the inverted matrix
  getinverseMatrix <- function() i
  
  #define calling functions
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverseMatrix = setinverseMatrix,
       getinverseMatrix = getinverseMatrix)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverseMatrix()
  
  ## in case the inverted Matrix is cached, then pick it up from here
  if (!is.null(i)) {
    message("getting cached data for Matrix")
    return(i)
  }
  
  # get the MAtrix 
  data <- x$getMatrix()
  
  #Native function call to caclulate the inverted Matrix
  i <- solve(data, ...)
  
  #return the inverted Matrix result into the object 
  x$setinverseMatrix(i)
  
  #return the Matrix
  i
}
