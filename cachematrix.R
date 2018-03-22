## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## Below are a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse matrix  
## of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix 
## has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the value
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
