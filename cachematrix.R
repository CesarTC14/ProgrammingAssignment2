## SET/GET/CACHE MATRIX INVERSE ##

## This function first tests if a matrix x is invertible, then creates a list of functions to allow caching the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        if(det(x) == 0) {
        stop(paste("matrix", "is", "not", "invertible", sep = " "))
        }
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inv) {
                m <<- inv
        }
        getinverse <- function() {
                m
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function gets the inverse of matrix x or simply return it if already calculated

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        else {
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        }
}
