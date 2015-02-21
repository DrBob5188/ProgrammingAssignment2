## This pair of functions aid in the reduction of expensive matrix calculations.
## makeCacheMatrix creates a 'cache matrix' encapsulating the matrix data type while
## exposing functions to get/set the internal matrix and its inverse.
## The companion function cacheSolve takes advantage of the exposed functions to retrieve
## the inverted matrix if present before performing the computationally expensive inversion.

## Encapsulates a matrix, adding helper functions to get/set the matrix and its inverse
## Returns a list of function pointers
## Persists the original matrix and it's inverse (if set).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <-function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the matrix data encapsulated in the supplied "cache matrix"
## if the inverse has been calculated it uses the cached value, otherwise it calculates and
## saves the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cachedinverse <- x$getinverse()
    if(!is.null(cachedinverse)) {
        message("Using Cached Data - Bonus cpu cycle savings!")
        return (cachedinverse)
    }
    cachedinverse <- solve(x$get())
    x$setinverse(cachedinverse)
    cachedinverse
}
