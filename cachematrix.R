## The makeCacheMatrix and cacheSolve functions facilitate caching
## and retrieving the inverse of a matrix.  The makeCacheMatrix function  
## creates a list of utility functions to retrieve and set cached values.
## The cacheSolve function uses the utility functions to retrieve the cached
## matrix inverse if it exists or set, cache and return the matrix inverse.
##
## Test Case:
## test.matrix <- matrix(c(1,0,3,2,2,4,3,2,1),ncol=3)
## solve(test.matrix) # result from cacheSolve(j) should match this result
## j<-makeCacheMatrix()
## j$set(test.matrix)
## cacheSolve(j)

## makeCacheMatrix: create list of get/set functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
	  ## set the matrix cache value
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
	  ## get the matrix cache value
        get <- function() x
	  ## set the matrix inverse cache value
        setinverse <- function(inverse) i <<- inverse
	  ## get the matrix inverse cache value
        getinverse <- function() i
	  ## create the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	  ## if the matrix inverse is cached, return it
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	  ## if the matrix inverse is not cached, set and
	  ## cache the matrix inverse and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
