## Put comments here that give an overall description of what your
## functions do

## The function "makeCacheMatrix" calculates the inverse of a matrix x.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
      # checking for the inverse in the cache
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      # if it is not in the cache it will get the data
      data <- x$get()
      # the inverse is calculated
      i <- solve(data, ...)
      # the inverse is cached
      x$setinverse(i)
      # the inverse matrix is returned
      i
}


## The function "cacheSolve" checks if the inverse has been calculated.
## If the inverse was already calculated: the inverse from the cache will be 
## presented computation. When the inverse can not be found in the cache, 
## the function will calculates the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
