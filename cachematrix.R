## Description of this file:
## This file contains two functions (makeCacheMatrix and cacheSolve).
## Together they can be used to avoid repeatedly calculating a matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
## This function creates a special matrix object that can cache its inverse
  inverse <- NULL
  set <- function(y) {  ## x and inverse are assigned values
    ## in an environment that is different from the current environment
    x <<- y       
    inverse <<- NULL
  }
  get <- function() x  ## return x
  setInverse <- function(M) inverse <<- M  ## inverse is assigned a value in an environment that is different from the current environment
  getInverse <- function() inverse ## return inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
  

## This function computes the inverse of the special matrix returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then `cacheSolve` simply retrieves the inverse from the cache.

cacheSolve <- function(x) {
   inverse<-x$getInverse()
   
   if(!is.null(inverse)) {   ## if the inverse has already been calculated, just return it
     message("getting cached inverse")
     return(inverse)
   }
   ## if the inverse has not been calculated, then get the matrix and invert it.
   M <- x$get()   #The matrix to invert
   inverse <- solve(M)
   x$setInverse(inverse)  # Cache the inverse.
   inverse  ## Return a matrix that is the inverse of 'x'
}
