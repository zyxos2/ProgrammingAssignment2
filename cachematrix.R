## Two functions for calculating inverse matrix from given matrix, and caching it value

## Creates an list containing functions:
## set - set matrix value
## get - get matrix value
## getsolve - get inverse matrix
## setsolve - set inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    ## set matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    ## return matrix
    get <- function() x
    ## store inverse matrix
    setinversion <- function(solve) s <<- solve
    ## get inverse matrix
    getinversion <- function() s
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}

## Calculate a matrix inversion; if inversion was calculated previously, then return cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinversion()
    ## Check if inverse matrix was calculated previously, if so, retrn cached value
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## calculate inverse matrix and return it
    data <- x$get()
    s <- solve(data, ...)
    x$setinversion(s)
    s
}
