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
   
    ## init the inverse variable
    inverse <- NULL
    
    ## creating the set function to set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
   
    ## creating the get function to retrieve the matrix
    get <- function() x
    
    ## creating the function storing the inversion of the matrix
    setInverse <- function(solve) inverse <<- solve
    
    ## creating the function to retrieve the inversion of the matrix
    getInverse <- function() inverse
    
    ## return a list of the created functions to work with 
    ## a matrix and its inversion
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Parameter x : the list with the functions to cache the inversion 
## of the matrix associated and additional parameters for the 
## solve function
##
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    ## getting the inversion of the matrix stored in x
    inverse <- x$getInverse()
    
    ## if inverse is not null, it means that 
    ## the inversion have been computed, so we return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## if inversion is null, then we have to compute the inversion
    ## we retrieve the matrix
    data <- x$get()
    message("computing data")
   
    ## we compute the inversion of the matrix with the solve function
    inverse <- solve(data, ...)
    
    ## we set the result. That we will be able to retrieve it 
    ## next time
    x$setInverse(inverse)
    
    ## the inversion of the matrix
    inverse
}
