## A pair of functions that cache the inverse of Matrix


## Creates a special "matrix" object that can be cached

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # Set function
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Get function
        get <- function() x
        
        # Set matrix object
        setmatrix <- function(matrix) m <<- matrix
        
        # Get matrix object
        getmatrix <- function() m
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix.
## Inverse is retrived from the cache if already calculated

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        
        # Return from cache if available
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
