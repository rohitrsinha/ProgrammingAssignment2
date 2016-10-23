## This is Programming Assignment 2: Lexical Scoping

## This function is a constructor that returns a matrix object
## with set and get functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(matrixInverse) inv <<- matrixInverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will takes input as matrix object and return inverse of
## matrix and cache the matrix inverse. It reurns cached inverse matrix on
## subsiquent calls.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
