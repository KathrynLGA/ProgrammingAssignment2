## This file creates two functions which create a special "matrix" object that can
## cache its inverse and compute the inverse if it has not been cached.

## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv2) inv <<- inv2
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the matrix returned by makeCacheMatrix as an argument
## It 1: checks to see if the inverse has been cached, 2: computes it if not
## 3: caches the computed inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
