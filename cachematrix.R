## Objective: Computing the inverse of a square matrix by exploiting the efficiency from caching the inverse value when it already exists


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("Cached data:")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data)
        x$setinverse(invs)
        invs
}
