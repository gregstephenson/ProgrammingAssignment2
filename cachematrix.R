## cachematrix.R contains functions to create a matrix object
## capable of caching its inverse to speed iterations through long lists.

## makeCacheMatrix creates a wrapper for a matrix object allowing caching
## of the inverse. It is assumed all entered matricies are invertible

makeCacheMatrix <- function(mat_x = matrix()) {
    ## Storage variable for matrix inverse
    inv <- NULL
    
    ## Set function - resets inv to null to prevent old inverse being returned
    ## for new matrix
    set <- function(mat_y) {
        mat_x <<- mat_y
        inv <<- NULL
    }
    
    ## Get stored matrix
    get <- function() mat_x
    
    ## Solve matrix inverse
    setinv <- function(solve) {
        inv <<- solve
    }
    
    ## Get matrix inverse
    getinv <- function() {
        inv
    }
    
    ## Provides object description
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve examines the matrix wrapper. If the inverse has not been computed
## cacheSolve solves the matrix, if it has, cacheSolve returns the cached version

cacheSolve <- function(mat_x, ...) {
    ## Retrieve inverse of matrix object wrapper
    inv <- mat_x$getinv()
    
    ## If a cached version exists, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If no cached version exists, retrieve the matrix...
    data <- mat_x$get()
    
    ## solve the matrix...
    inv <- solve(data, ...)
    
    ## store the solved matrix in the cache...
    mat_x$setinv(inv)
    
    ## and return it as required
    inv
}
