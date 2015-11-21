## Since it is often difficult and time consuming to compute
## the inverse of a matrix, the following two functions create
## an arbitrary square matrix and compute its inverse. For this
## reason, it is easier to cache the matrix

## The first function below creates and caches a square matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This next function takes the cached matrix from the function
## makeCachematrix and computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## Get cached matrix
    inv <- x$getinv()
    ## Retrieve cached matrix if inverse has already been calculated
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    ## Compute inverse of cached matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    # Return inverse
    inv
}
