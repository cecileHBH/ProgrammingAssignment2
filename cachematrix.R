## Functions to put in cache a matrix inversion.
## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## This function create a list containing the functions that will
## able to cache the matrix inversion
##
## Parameter x : a matrix needing to be cached
##
## Return a list with the functions to cache the matrix x
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Parameter x : the list with the function to compute the inversion 
## of the matrix associated and additional parameter for the 
## solve function
##
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    message("computing data")
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
