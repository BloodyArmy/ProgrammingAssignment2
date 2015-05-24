## Coursera - R Programming - Programming Assignment 2: Lexical Scoping
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion but won't be discussed here).
## Thus, creating 2 functions - makeCacheMatrix and cacheSolve for the objective above. 

## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize inversedMatrix to later store inverse matrix data
  inversedMatrix <- NULL
  
  ## Method - set the current matrix and set inversedMatrix to NULL
  set <- function(y) {
    originalMatrix <<- y 
    inversedMatrix <<- NULL
  }
  
  ## Method - get the current matrix
  get <- function() originalMatrix ## return the original matrix
  
  ## Method - set the inverse matrix
  setInverse <- function(matrix) inversedMatrix <<- matrix ## cache the returned inversed matrix
  
  ## Method - get the inverse matrix
  getInverse <- function() inversedMatrix ## return the inversed matrix

  ## Returns the list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## This function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
        
  ## Get cached inverse matrix of x and store to tempMatrix, tempMatrix can be null or otherwise.
  tempMatrix <- x$getInverse()
  
  ## If tempMatrix is not null, return as final answer.
  if (!is.null(tempMatrix)){
    
    message("getting cached data")
    return(tempMatrix)
    
  } else {
    
    ## If tempMatrix is null, calculate x inverse matrix as tempMatrix and store to cache, then return as final answer.
    tempMatrix <- solve(x$get())
    x$setInverse(tempMatrix)
    return(tempMatrix) 
    
  }
  
}
