## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this functio
makeCacheMatrix <- function(x = matrix()) { 
  ## matrix inverse value set to NULL
  matinv <- NULL                     
  set <- function(y = matrix()) {                      
    x <<- y
    matinv <<- NULL              
  }
  ##get the matrix
  get <- function() x                           
  #find the inverse
  setinverse <- function(solve) matinv <<- solve 
  # get the inverse     
  getinverse <- function() matinv        
  ## passes the value of the function makeCacheMatrix        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve<- function(x, ...) {                 
  matinv <- x$getinverse()
  #if the inverse exists, it gets it.
  if(!is.null(matinv)) {                 
    return(matinv)
  }
  z <- x$get()                               
  matinv <- solve(z, ...)
  x$setinverse(matinv)
  matinv
}
