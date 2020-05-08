##  The makeCacheMatrix and cacheSolvefunctions create an object that stores a matrix and creates a cache of its inverse

## The makeCacheMatrix function creates a matrix which is a list of functions that 
## set the value of the matrix, get the value of the matrix, set the value of the inverse 
## and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The cacheSolve function calculates the inverse of the matrix created by makeCacheMatrix. 
## First it checks whether the inverse has been calculated already and, if it has, it gets the inverse from the cache, 
## skipping the calculation. If it has not already been calculated, it calculates the matrix's inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  
}


