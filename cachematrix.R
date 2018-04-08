## Function: cachematrix.R (Pprogramming Assignment 2)

## These functions calculate the inverse of a matrix.
## Values can be retrieved from cache or previously calculated. 

## Create vector to store the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # inv = inverse of the matrix
    inv <- NULL
    # set = matrix for parameter y
    set <- function(y) {
        ##check environment to set matrix to y reset inv to NULL
        x <<- y
        inv <<- NULL
    }
  
    ## Get matrix
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    ## Return list containing set, get, setinverse and getinverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## If exists -> pull value from cache; else compute

cacheSolve <- function(x, ...) {
    ## Return a matrix that is inverse of x
    ## Get cached inverse of matrix
    inv <- x$getinv()
    if(!is.null(inv)) {
        ## if exists -> retrieve cached inverse
        message("getting cached data")
        return(inv)
    }

    ## if not exist -> compute inverse  
    data <- x$get()
    inv <- solve(data, ...)
    ## add inverse to cache
    x$setinv(inv)
    ## Return matrix for inverse of x
    inv
}
