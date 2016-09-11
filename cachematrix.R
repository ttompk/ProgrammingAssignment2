## Created for coursera R programming course assignment
## This script contains 2 functions. The first function caches the value
## of a matrix within the environment of the function. Has 4 functions within
## its scope (see list). The second function inverses an invertable square 
## matrix, e.g. matrix(c(1,2,2,1),nrows=2).
## 

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y) {
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setreverse <- function(solve) r <<- solve
        getreverse <- function() r
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getreverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setreverse(m)
        m
}
