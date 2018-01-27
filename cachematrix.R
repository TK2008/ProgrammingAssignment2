## makeCacheMatrix and cacheSolve functions are written to calculate and cache the inverse of matrix
## We assume that the matrix supplied as an argument, is always invertible

## makeCacheMatrix creates a mattix object whitch caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinv <- function(invmtrx) invm <<- invmtrx
    getinv <- function() invm
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the matrix inverse returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invm <- x$getinv()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinv(invm)
    invm
}