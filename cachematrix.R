## Write a pair of functions that cache the inverse of a matrix.
## These functions should be able to cache potentially time-consuming computations.
## 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  
    i <- NULL            ## initializing i for inverse and setting it to NULL value
    ##set value of a matrix
    set <- function(y) { 
        x <<- y         ## <<- assigning a value to an object in an environment that is
                        ## different from the current environment
        i <<- NULL
    }
    ## get value of a matrix
    get <- function() x
    setinverse <- function(inverse) i <<- inverse   ## set value of an inverse
    getinverse <- function() i                      ## get value of an inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## assume that the matrix supplied is always invertible.
        ## Computing the inverse of a square matrix can be done with the solve function in R.

    ## checks to see if the inverse has already been calculated.
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()          ## if inverse is calculated gets the inverse from the cache 
                             ## and skips the computation.
    i <- solve(data, ...)    ## if inverse is not calculated calculates the inverse of the data
                             ##if X is a square invertible matrix, then solve(X) returns its inverse.
    x$setinverse(i)          ## sets the value of the inverse in the cache via the setinverse function
    i
}