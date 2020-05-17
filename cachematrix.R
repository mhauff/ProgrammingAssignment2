## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Uses symbols defined in the parent environment to make a matrix that can cahce itself.
makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x
         setInverse <- function(vectorInverse) inv <<- vectorInverse
         getInverse <- function() inv
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## Calculates the inverse of the cahced matrix. If matrix changes then the inverse is 
## calculated once more. Use solve when dealing with sqare matrices.
## tried different values at times but hit an issue with singularity
## Works with test_mat <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve <- function(x, ...) {
        ## Return opposite of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Retrieving Cached Data")
                return(inv)
        }
        testMat <- x$get()
        inv <- solve(testMat, ...)
        x$setInverse(inv)
        inv
}
