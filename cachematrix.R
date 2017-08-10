## Overall this pair of functions can be used to cache the result of the inverse
## of a a matrix, such that if you subsequently request the same result,
## cached data will be retrieved and printed to the console, rather than
## re-calculating the result



## OK so the first function makes a cache of the matrix
## and also creates the set, get, setinv, and getinv
## functions to be called by cacheSolve (in the case
## of the latter three) or from the console.

## The syntax is makeCacheMatrix(matrix1), where
## matrix1 is a matrix.

makeCacheMatrix <- function(x = matrix()) {
        my_inv <- matrix(NA)
        set <- function (y) {
                x <<- y
                my_inv <<- matrix(NA)
        }
        get <- function() x
        setinv <- function(solve) my_inv <<- solve
        
        getinv <- function() my_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The argument to this function should be the list
## e.g. if you do my_cached_matrix <- makeCacheMatrix(matrix1), where matrix 1 is
## a matrix, then this function should be called with the syntax
## cacheSolve(my_cached_matrix)

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(length(m)>1) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m        
}