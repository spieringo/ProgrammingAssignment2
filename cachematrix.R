## This pair of functions is used to create a list of objects
## that can hold a matrix and its inverse.
## This list is useful because once the inverse of a matrix
## is computed, its value can be cached. 
## Using this strategy, the user never needs to calculate
## a inverse of the same matrix twice.

## This function create an object (a list) 
## to store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function is used to compute the inverse matrix
## of a matrix contained within a "makeCacheMatrix"
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)){
        message("getting cache data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}