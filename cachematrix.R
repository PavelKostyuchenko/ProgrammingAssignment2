## The function creates a list with 4 arguments - collective functions for inverse and
##results of inverse for matrices already processed.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## Write a short comment describing this function
## The function below checks if a matrix has already been inversed.
## If yes, the resultes is taked from cache, otherwise - evaluates using solve() function

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("already have cached data")
    return(m)
  }
  data <- x$getMatrix()
  m<-solve(data, ...)
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}