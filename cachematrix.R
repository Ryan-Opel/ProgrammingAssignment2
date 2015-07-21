# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than computing 
# it repeatedly. This pair of functions will do exactly that by utilizing
# a "special" variable, a list containing a function.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         # Setting inverse matrix to empty
        invMat <- NULL 
        # Sets matrix and default values for invMat 
        setMat <- function(matrix) {
                x <<- matrix 
                invMat <<- NULL # Returns inverse matrix to empty
        }
        # Gets matrix
        getMat <- function() x # Returns matrix
        # Sets inverse matrix
        setInvMat <- function(inverse) invMat <<- inverse
        # Gets inverse matrix
        getInvMat <- function() invMat
        # List of the operations performed by the function
        list(setMat = setMat, getMat = getMat,
             setInvMat = setInvMat,
             getInvMat = getInvMat)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.


cacheSolve <- function(x, ...) {
        # Returns the inverted matrix
        invMat <- x$getInvMat()
        # If cache exists, returns it, otherwise, calculates the inverse
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        # Passes matrix data to matrix variable
        matrix <- x$getMat()
        # Performs the inversion operation
        invMat <- solve(matrix, ...)
        # Stores data to cache and returns the value
        x$setInvMat(invMat) 
        invMat
}
