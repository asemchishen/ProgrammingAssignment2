## This fuction is designed to be used as caching analog of original 
## solve() function.Once inverse matrix is calculated for the first 
## time it is stored in cache for next usage. Function consist of
## two functions:
## 1. makeCacheMatrix() takes matrix as an argument and store its value
## and value of inverse matrix to it.
## 2. casheSolve() takes the result of makeCacheMatrix() and retrive 
## inverse matrix cached value from makeCacheMatrix() environment.



## this takes matrix as an argument and store its value
## and value of inverse matrix to it

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This takes the result of makeCacheMatrix() and retrive 
## inverse matrix cached value from makeCacheMatrix() environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
