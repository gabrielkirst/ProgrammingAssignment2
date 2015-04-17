## Put comments here that give an overall description of what your
## functions do
# The following two functions are used to cache the inverse of a matrix.

## Write a short comment describing this function
# This function set and get the value of the matrix, and also 
# set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## Write a short comment describing this function
# This function assumes that the matrix is always invertible.
# Return the inverse of the matrix, first checking if the inverse was already 
# computed.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
