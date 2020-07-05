## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix creates a list containing a function to 
##1. set the value of the matrix 2. get the value of the matrix
##3. set the inverse of the matrix 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(matrixInverse) m <<- matrixInverse
        getMatrixInverse <- function() m
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixInverse(m)
        m
}
