## Functions for caching the inverse of a matrix

## Caches the matrix 'x' and returns a list of functions
## to cache and retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Intialize inverese to NULL
    xinv <- NULL

    # Set new matrix and re-set inverse to NULL
    setmatrix <- function(y) {
        x <<- y
        xinv <<- NULL
    }

    # Returns the matrix object
    getmatrix <- function() x

    # Caches the inverse
    setinverse <- function(inv) xinv <<- inv

    # Returns the inverse of the matrix
    getinverse <- function() xinv
    
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a matrix 'x',
## created with makeCacheMatrix() function
##
## Use cached data if available,
## otherwise solve for the inverse and cache it

cacheSolve <- function(x, ...) {
    # Get cached inverse
    xinv <- x$getinverse()

    # If cached inverse is available, return it
    if(!is.null(xinv)) {
        message("Getting cached inverse")
        return(xinv)
    }

    message("No cached result avaiable, solving for inverse")

    # If cached inverse isn't available,
    # solve for the inverse
    mtmp <- x$getmatrix()
    xinv <- solve(mtmp, ...)

    # Cache the inverese and return it
    x$setinverse(xinv)
    xinv
}
