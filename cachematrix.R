## This source contains cachable matrix inversion functions.
## Usually matrix inversion a costly computation
## so caching results allows don't repeat computations
## with the same data.

## Create cachable matrix from given matrix
## or empty matrix as default.

makeCacheMatrix <- function(x = matrix()) {
    inverseCached <- NULL
    set <- function(y) {
        x <<- y
        inverseCached <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inverseCached <<- solve
    getsolve <- function() inverseCached
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Returns cached inversed matrix
## or compute it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    m <- x$get()
    s <- solve(m)
    x$setsolve(s)
    s
}
