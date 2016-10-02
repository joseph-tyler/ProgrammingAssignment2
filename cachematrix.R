## These functions are involved in inverting a matrix.  If the inversion
## is already calculated, it is simply returned.

## makeCacheMatrix creates a matrix object that can cache its inverse
## set() sets the value of the matrix
## get() gets the values of the matrix
## setinverse() sets the matrix inversion
## getInverse() gets the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(i) m <<- solve(x)
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix object made by
## makeCacheMatrix, or retrieves an inverse if already calculated

cacheSolve <- function(x, ...) {
        r <- x$getinverse()
        if(!is.null(r)) {
            return(r)
        }
        data <- x$get()
        r <- solve(data)
        x$setinverse(r)
        r
}
