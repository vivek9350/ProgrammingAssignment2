##The purpose of this code is to write two functions, namely,
#"makeCacheMatrix" and "cacheSolve". 
#The functions makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Below function is cachesolve function which computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
