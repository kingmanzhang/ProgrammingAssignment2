## Project 3
## Functions that solve the inverse of a matrix with cached data or compute from scratch

## A function that creates an environment for holding data and function bindings

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## A function that solves the inverse of a matrix with cached data or compute from scratch.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

## how to run the above functions
m <- makeCacheMatrix()
set.seed(7832)
m$set(matrix(rnorm(25), ncol = 5))
cacheSolve(m) # compute from scratch
cacheSolve(m) # return cached answer

m$set(matrix(rnorm(16), ncol = 4))
cacheSolve(m) # compute from scratch
cacheSolve(m) # return cached answer
