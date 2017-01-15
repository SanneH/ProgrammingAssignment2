## R function able to cache potentially time-consuming computations, i.e. in that case creates a special "matrix" object 
## that can cache its inverse

## This function returns a list containing functions to set and get a matrix as well as set and get the inverse of the matrix. 
## The list is used as input to cachesolve()

makeCacheMatrix <- function(x = matrix()) {
## x is a square invertible matrix
     
    invmtx <- NULL
    set <- function(y) {
        x <<- y
        invmtx <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invmtx <<- inverse
    getinv <- function() invmtx
    
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmtx <- x$getinv()
    
    ## if the inverse has already been calculated
    if(!is.null(invmtx)) {
        message("getting cached data")
        return(invmtx)
    }
    ## otherwise, calculates the inverse
    data <- x$get()
    invmtx <- solve(data)
    ## sets the value of the inverse in the cache via the setinv function
    x$setinv(invmtx)
    invmtx
}
