## These functions return inverse of a matrix passed as parameter
## It will return cached inverse if it was calculated before
## to run the example:
##> source("cachematrix.R")
##> z<-makeCacheMatrix(x) where x is a matrix 
##> cacheSolve(z)

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## After a matrix is set it will be stored in an environment that is different from 
## the current environment (<<- operator)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(func) m <<- func
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) #%*% data ##Identity Matrix
    x$setinverse(m)
    m
}
