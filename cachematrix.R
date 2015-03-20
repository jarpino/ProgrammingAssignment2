## These functions allows you to create a matrix and cache it in memory for user later and
## also to solve for the inverse of the matrix and return it from memory if it
## has already been solved for. Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## store the matrix
  internalCacheMatrix <- x
  
  ## clear the cached inverse matrix
  internalCacheInverseMatrix <- NULL
  
  ## get the matrix from cache
  getMatrix <- function() internalCacheMatrix
  
  ## set a new matrix from cache
  setMatrix <- function(s = NULL) {
    
    internalCacheMatrix <<- s
    internalCacheInverseMatrix <<- NULL
    
  }
  
  ## set the new inverse matrix in memory for recall later
  setInverse <- function(newInverse = matrix()) internalCacheInverseMatrix <<- newInverse
  
  ## get the inverse matrix from memory
  getInverse <- function() internalCacheInverseMatrix
  
  ## return list of functions 
  list(get = getMatrix, set = setMatrix,
       getInverse = getInverse, setInverse = setInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(passedCache, ...) {
        
  ## get the chached matrix
  c <- passedCache$getInverse()
  
  ## check to see if there is a cached value, if so, return it.
  if(is.null(c)) {
    
    message("No previously cached value, generating.")
    
    ##pass the stored matrix to the solve function then store its inverse
    passedCache$setInverse(solve(passedCache$get()))
    
    ##return the inverse matrix that has been stored
    print(passedCache$getInverse())

  } else {
    
    ## Return the cached inverse matrix
    print(c)
    
  }
  
}
