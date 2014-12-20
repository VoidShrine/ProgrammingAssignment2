## Compute the inverse of an invertible matrix. If the results have been
## calculated before, return the cached values.

## Return a list that has four sub-functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set a new matrix
    set <- function(y) {
                x <<- y
                m <<- NULL
    }

    ## Get the value of current matrix
    get <- function() x

    ## Set the inverse matrix of current matrix
    setinverse <- function(inverse) m <<- inverse

    ## Get the cached inverse matrix
    getinverse <- function() m

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Read a matrix and compute its inverse. Skip the calculation if the results
## are found in the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
