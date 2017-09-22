## The below functions are designed to solve and return the inverse of a matrix.
## If the matrix is already inversed then it is simply returned.

## The 'makeCacheMatrix' function is, first, defining a matrix, 
## then creates functions to set and get the matrix and finally
## sets and gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The 'cacheSolve' function returns the inverse of the matrix defined, 
## unless the inverse has already been calculated in which case the matrix 
## is simply returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


