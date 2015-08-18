## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. This pair 
## of functions cache the inverse of a matrix.
##
## To use the functions, code as follows, where "mat" is a square invertible matrix:
##      > xxx <- makeCacheMatrix(mat)
##      > cacheSolve(xxx)

## This function creates a special "matrix" object that can cache its inverse. This is a
## list of four functions to set/get the value of the matrix and the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache. Otherwise it finds the
## inverse using the solve() function and caches it via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
