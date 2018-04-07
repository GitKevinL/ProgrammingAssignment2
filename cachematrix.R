## Function: cachematrix.R (Pprogramming Assignment 2)

## These functions calculate the inverse of a matrix.
## Values can retrieved from cache ir previously calculated. 

## Create vector to store the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
  
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## If exists -> pull value from cache; else calculate

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
  
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix for inverse of 'x'
    inv
}
