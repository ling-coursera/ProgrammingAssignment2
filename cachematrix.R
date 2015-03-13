## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function () x
    
    setsolve <- function(invs) inv <<- invs
    
    getsolve <- function() inv
    
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data)
    x$setsolve(inv)
    inv
}
