## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions that stores a matrix and its
## inverse.
## The function uses the <<- operator to make internal variables accessible.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function sets the inverse matrix in the list created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    ## Check to see whether matrix is invertible
    if (det(data) == 0) {
        message("matrix is noninvertible")
    } else {
        i <- solve(data, ...)
        x$setinverse(i)
        i
    }
}
