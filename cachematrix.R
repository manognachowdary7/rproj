## inverse of a matrix

## the function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverse <<- solve
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Thisfunction computes the inverse of the matrix
## returned by "makeCacheMatrix" above. If the inverse has already been calculated (the same matrix), then
## this function can retrive the inverse from the cache

cachesolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
