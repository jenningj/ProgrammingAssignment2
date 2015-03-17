## function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
# Sets the value of the matrix
# Gets the value of the Matrix
# Sets the inverse of the matrix
# Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setMatrix = setmatrix,
       getMatrix = getmatrix)
}


## Write a short comment describing this function
##  computes the inverse of the special "matrix" returned by makeCacheMatrix 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # If X is a square invertible matrix, then solve(X) returns its inverse.
  # ( assume that the matrix supplied is always invertible)
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix,...)
  x$setmatrix(m)
  return(m)
}

makeCacheMatrix
cacheSolve
