## Put comments here that give an overall description of what your
## functions do

## The function "makeCacheMatrix" calculates the inverse of a matrix x.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      # get the value of the matrix
      get <- function() x
      # set the value of the inverse
      setinverse <- function(inv) i <<- inv
      # get the value of the inverse
      getinverse <- function() i
      # list of all the above functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The function "cacheSolve" checks if the inverse has been calculated.
## If the inverse was already calculated: the inverse from the cache will be 
## presented. When the inverse can not be found in the cache
## the function will calculate the inverse of the matrix. 

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




